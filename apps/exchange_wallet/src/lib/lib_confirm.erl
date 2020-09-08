%%%--------------------------------------
%%% @Module  : lib_confirm
%%% @Description: 确认相关处理
%%%--------------------------------------
-module(lib_confirm).

-export([do_chain/1]).

-include_lib("exchange_common/include/common.hrl").

-include("exchange_wallet.hrl").

-define(URL_DEPOSIT_NOTIFY, "depositNotify").
-define(URL_WITHDRAW_NOTIFY, "withdrawNotify").


do_chain(Chain) ->
    %?DBG("processing confirm for chain ~s...~n", [Chain]),
    try
        TodoTxList = lib_tx_store:get_todo_tx_list(Chain),
        NeedConfirmCount = lib_chain_cfg:confirm_count(Chain),
        [do_tx(Chain, Tx, NeedConfirmCount) || Tx <- TodoTxList],
        ok
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("confirm exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

do_tx(Chain, #{<<"_id">> := TxId0,
               <<"trans_id">> := TransId,
               <<"is_withdraw">> := IsWithdraw,
               <<"from_addr">> := FromAddrList,
               <<"to_addr">> := ToAddr,
               <<"symbol">> := Symbol,
               <<"amount">> := Amount,
               <<"real_fee">> := RealFee,
               <<"is_internal">> := IsInternal} = OldTx,
      NeedConfirmCount) ->
    try
        [TxId|_] = binary:split(TxId0, <<"__">>),
        % 得到最新的tx确认状态
        case lib_chain:get_tx(Chain, TxId) of
            {_FromAddrList, ToAddrList, Symbol, _AmountList, _Gas, Confirmations} ->
                true = lists:any(fun(A) ->
                                     % 采用前缀匹配方式来校验，因为AChain的ToAddr可能是子地址
                                     case binary:match(ToAddr, [A], []) of
                                         {0, _} -> true;
                                         _ -> false
                                     end
                                 end, ToAddrList), % 相当于断言
                case Confirmations < NeedConfirmCount of
                    true ->
                        % 确认数仍旧未达到，更新tx确认数
                        lib_tx_store:upd_tx(Chain, OldTx#{<<"confirm">> := Confirmations});
                    false ->
                        % 确认数已达到，先更新tx确认数，然后再通知，通知成功后再更新tx的notify状态
                        lib_tx_store:upd_tx(Chain, OldTx#{<<"confirm">> := Confirmations}),
                        % 再更新一遍余额，因为IPC链，转账所引用的utxo在8个确认数内被雪藏，所以提币时立即更新的余额实际是错误的，这里需要重新刷新余额
                        [lib_chain_balances:update_balance(Chain, Symbol, FromAddr) || FromAddr <- FromAddrList],
                        notify(Chain, TxId, trunc(TransId), trunc(IsWithdraw), FromAddrList, ToAddr, Symbol, Amount, RealFee, Confirmations),
                        lib_tx_store:upd_tx(Chain, OldTx#{<<"confirm">> := Confirmations, <<"notify_finance">> := 1})
                end;
            undefined -> % 在链上没有找到，可能是临时分叉所导致
                case IsWithdraw =:= 1 andalso IsInternal =:= 0 of
                    true ->
                        % 防止延展性攻击，不自动处理，手动处理
                        % % 先恢复余额
                        % lib_tx_store:upd_tx(Chain, OldTx#{<<"is_invalid">> := 1}),
                        % [lib_chain_balances:update_balance(Chain, Symbol, FromAddr) || FromAddr <- FromAddrList],
                        % % 等待withdraw进程从finance模块拉取并重新发起
                        ok;
                    false -> void
                end;
            temporarily_not_found ->
                ?ERR("temporarily not found in chain: trans_id=~p~n", [TransId]),
                void;
            _ ->
                ?ERR("the data in tx store does not match the data in chain: trans_id=~p~n", [TransId]),
                void
        end,
        ok
    catch
        _:Err ->
            ?ERR("do_tx error(chain=~p, tx_id=~p):~nerr_msg=~p~nstack=~p~n", [Chain, TxId0, Err, erlang:get_stacktrace()]),
            {exception, ?T2B(Err)}
    end.

notify(Chain, TxId, TransId, 1 = _IsWithdraw, FromAddrList, ToAddr, Symbol, Amount, RealFee, Confirmations) ->
    case TransId of
        0 -> void; % 汇总，不必通知finance
        _ ->
            AppId = lib_finance_request:get_app_id(),
            AmountBin = float_to_binary(Amount, [{decimals, 8}, compact]),
            RealFeeBin = float_to_binary(RealFee, [{decimals, 8}, compact]),
            TransIdBin = integer_to_binary(TransId),
            BinToSign = <<"address_to=", ToAddr/binary,
                          "&amount=", AmountBin/binary,
                          "&app_id=", AppId/binary,
                          "&confirm=", (integer_to_binary(Confirmations))/binary,
                          "&real_fee=", RealFeeBin/binary,
                          "&symbol=", Symbol/binary,
                          "&trans_id=", TransIdBin/binary,
                          "&txid=", TxId/binary>>,
            Params = [{address_to, ToAddr},
                      {amount, AmountBin},
                      {app_id, AppId},
                      {confirm, Confirmations},
                      {real_fee, RealFeeBin},
                      {symbol, Symbol},
                      {trans_id, TransId},
                      {txid, TxId}],
            Url = lib_finance_request:get_url(?URL_WITHDRAW_NOTIFY),
            case lib_finance_request:do(Url, BinToSign, Params) of
                {ok, Res} ->
                    case lib_chain_addrs:is_addr_ours(Chain, ToAddr) of
                        true ->
                            lib_chain_balances:update_balance(Chain, Symbol, ToAddr),
                            notify(Chain, TxId, TransId, 0, FromAddrList, ToAddr, Symbol, Amount, RealFee, Confirmations);
                        false -> void
                    end,
                    {ok, Res};
                Error -> throw(Error)
            end
    end;
notify(Chain, TxId, _TransId, 0 = _IsWithdraw, FromAddrList, ToAddr, Symbol, Amount, _RealFee, Confirmations) ->
    case is_fee_charge(Chain, FromAddrList) of
        true -> void; % 手续费地址转来的，不能通知finance
        false ->
            AppId = lib_finance_request:get_app_id(),
            AmountBin = float_to_binary(Amount, [{decimals, 8}, compact]),
            Now = util:longunixtime(),
            BinToSign = <<"address_to=", ToAddr/binary,
                          "&amount=", AmountBin/binary,
                          "&app_id=", AppId/binary,
                          "&confirm=", (integer_to_binary(Confirmations))/binary,
                          "&is_mining=0",
                          "&symbol=", Symbol/binary,
                          "&timestamp=", (integer_to_binary(Now))/binary,
                          "&txid=", TxId/binary>>,
            Params = [{address_to, ToAddr},
                      {amount, AmountBin},
                      {app_id, AppId},
                      {confirm, Confirmations},
                      {is_mining, 0},
                      {symbol, Symbol},
                      {timestamp, Now},
                      {txid, TxId}],
            Url = lib_finance_request:get_url(?URL_DEPOSIT_NOTIFY),
            case lib_finance_request:do(Url, BinToSign, Params) of
                {ok, Res} -> {ok, Res};
                Error -> throw(Error)
            end
    end.

is_fee_charge(Chain, [FromAddr]) ->
    case lib_chain_addrs:get_addrs_by_type(Chain, ?ADDR_TYPE_FEE) of
        [FeeAddr|_] -> % 手续费地址只有一个
            FromAddr =:= FeeAddr;
        _ ->
            false
    end;
is_fee_charge(_Chain, _FromAddrList) ->
    false.
