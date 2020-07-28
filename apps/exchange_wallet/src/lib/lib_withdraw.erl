%%%--------------------------------------
%%% @Module  : lib_withdraw
%%% @Description: 提币相关处理
%%%--------------------------------------
-module(lib_withdraw).

-export([do_chain/1]).
-export([get_withdraw_tasks/2]).

-include_lib("exchange_common/include/common.hrl").

-include("exchange_wallet.hrl").

-define(URL_WITHDRAW_CONSUME, "withdrawConsume").


do_chain(Chain) ->
    %?DBG("processing withdraw for chain ~s...~n", [Chain]),
    Symbols = lib_chain:get_symbols_by_chain(Chain),
    [do_symbol(Chain, Symbol) || Symbol <- Symbols].

do_symbol(Chain, Symbol) ->
    try
        {ok, Tasks} = get_withdraw_tasks(Symbol, 10),
        [do_task(Chain, Task) || {Task} <- Tasks],
        ok
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("withdraw exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

get_withdraw_tasks(Symbol, Num) ->
    AppId = lib_finance_request:get_app_id(),
    BinToSign = <<"app_id=", AppId/binary,
                  "&count=", (integer_to_binary(Num))/binary,
                  "&symbol=", Symbol/binary>>,
    Params = [{app_id, AppId},
              {count, Num},
              {symbol, Symbol}],
    Url = lib_finance_request:get_url(?URL_WITHDRAW_CONSUME),
    case lib_finance_request:do(Url, BinToSign, Params) of
        {ok, Tasks} -> {ok, Tasks};
        Error -> throw(Error)
    end.

do_task(Chain, Task) ->
    try
        {_, TransId} = lists:keyfind(<<"trans_id">>, 1, Task),
        case lib_tx_store:does_trans_id_exist(Chain, TransId) of
            true ->
                ?INFO("withdraw task has already been in tx table: chain=~s, trans_id=~p~n", [Chain, TransId]),
                void;
            false ->
                {_, Symbol0} = lists:keyfind(<<"symbol">>, 1, Task),
                {_, ToAddress} = lists:keyfind(<<"address_to">>, 1, Task),
                {_, Amount0} = lists:keyfind(<<"amount">>, 1, Task), % Amount已刨除掉了Fee
                {_, Fee0} = lists:keyfind(<<"fee">>, 1, Task),
                Amount = util:round10d(Amount0),
                Fee = util:round10d(Fee0),
                true = (Amount > 0 andalso Fee > 0), % 相当于断言
                Symbol = list_to_binary(string:to_lower(binary_to_list(Symbol0))),
                {Gas, GasSymbol} = lib_chain_cfg:gas(Symbol, Amount),
                case Gas > Fee of
                    true ->
                        throw({error, <<"gas higher than fee: gas=", (?T2B(Gas))/binary, ", fee=", (?T2B(Fee))/binary>>});
                    false -> void
                end,
                NeedAmount = case Symbol =:= GasSymbol of
                                 true -> util:round10d(Amount + Gas);
                                 false -> Amount
                             end,
                % 根据币种找到最接近的余额地址
                case lib_chain_balances:find_addrs_with_fittest_balance(Chain, Symbol, NeedAmount) of
                    [] ->
                        throw({error, <<"balance not enough:", Chain/binary, ",", Symbol/binary, ",", (float_to_binary(NeedAmount))/binary>>});
                    AddrToDeltaAmountPairs0 ->
                        NeedAmount = util:round10d(lists:sum([DeltaAmount || {_, DeltaAmount} <- AddrToDeltaAmountPairs0])), % 相当于断言
                        AddrToDeltaAmountPairs =
                            case Symbol =:= GasSymbol orelse Gas =< 0 of
                                true -> AddrToDeltaAmountPairs0;
                                false ->
                                    % 对于需要汇总的链而言，因为汇总地址只有一个，因此找到的是同一个地址
                                    case lib_chain_balances:find_addrs_with_fittest_balance(Chain, GasSymbol, Gas) of
                                        [] ->
                                            throw({error, <<"gas balance not enough:", Chain/binary, ",", GasSymbol/binary, ",", (float_to_binary(Gas))/binary>>});
                                        AddrToDeltaAmountPairsGas0 ->
                                            % 将Gas的数量变成负值，只是为了区分
                                            AddrToDeltaAmountPairsGas = [{Addr, -DeltaAmount} || {Addr, DeltaAmount} <- AddrToDeltaAmountPairsGas0],
                                            AddrToDeltaAmountPairs0 ++ AddrToDeltaAmountPairsGas
                                    end
                            end,
                        do_withdraw(TransId, Chain, Symbol, ToAddress, AddrToDeltaAmountPairs, Amount, Fee, Gas, GasSymbol)
                end
        end,
        ok
    catch
        _:Err ->
            TransactionId = case lists:keyfind(<<"trans_id">>, 1, Task) of
                                {_, Tid} -> Tid;
                                _ -> unknown
                            end,
            ?ERR("withdraw error(chain=~p, trans_id=~p):~nerr_msg=~p~nstack=~p~n", [Chain, TransactionId, Err, erlang:get_stacktrace()]),
            {exception, ?T2B(Err)}
    end.

do_withdraw(TransId, Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Amount, Fee, Gas, GasSymbol) ->
    {FromAddrList, DeltaAmountList} = lists:unzip(AddrToDeltaAmountPairs),
    % 送往签名节点去生成签名的交易
    case lib_sign_node:make_tx(Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas) of
        SignedTx when is_binary(SignedTx) ->
            % 发送交易
            TxId = lib_chain:send_tx(Chain, SignedTx),
            % 加入tx表，当周期扫描各交易的确认数达到预期后，发送通知给finance模块
            lib_tx_store:new_tx(Chain, TxId, [TransId, 1, FromAddrList, ToAddr, Symbol, Amount, Fee, Gas, DeltaAmountList]),
            % 提币，立即更新余额
            case Symbol =:= GasSymbol of
                true ->
                    [lib_chain_balances:update_balance(Chain, Symbol, FromAddr) || FromAddr <- FromAddrList];
                false ->
                    [case DeltaAmount > 0 of
                         true -> lib_chain_balances:update_balance(Chain, Symbol, FromAddr);
                         false -> lib_chain_balances:update_balance(Chain, GasSymbol, FromAddr)
                     end || {FromAddr, DeltaAmount} <- AddrToDeltaAmountPairs]
            end;
        Res ->
            throw(Res)
    end,
    ok.

