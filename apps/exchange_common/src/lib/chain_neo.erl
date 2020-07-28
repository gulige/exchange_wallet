%%%--------------------------------------
%%% @Module  : chain_neo
%%% @Description: 小蚁链的访问接口
%%%--------------------------------------
-module(chain_neo).
-compile(export_all).

-export([start_node/0,
         stop_node/0,
         need_gather/0,
         get_new_address/0,
         get_balance/2,
         make_tx/4,
         sign_tx/2,
         send_tx/1,
         get_latest_block_id/0,
         get_block/1,
         get_tx/1,
         import_priv_key/1]).

-include("common.hrl").

-define(CHAIN, <<"neo">>).
-define(SYMBOL_NEO, <<"neo">>).
-define(SYMBOL_GAS, <<"gas">>).
-define(FACTOR, 1).

-define(ASSETID_NEO, <<"c56f33fc6ecfcd0c225c4ab356fee59390af8560be0e930faebe74a6daff7c9b">>).
-define(ASSETID_GAS, <<"602c79718b16e442de58778e148d0b1084e3b2dffd5de6b7b16cee7969282de7">>).


start_node() ->
    ?INFO("starting neo node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/neo; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/neo/stop.sh"),
    ok.

need_gather() ->
    % 小蚁链的全局资产（NEO、GAS等）使用UTXO模型，类似比特币，可以多个地址一起转账，因此不需要余额汇总
    % 小蚁链的合约资产使用BALANCE模型，需要余额汇总，不过，小蚁链内部管理了这些余额计算，转账时可以不用提供From地址，因此我们不需要维护余额，也不需要汇总
    % 在转账或提现时，NEO钱包会从一个或多个地址中找到即能满足需求又使用总输入最小的零钱作为本次交易的输入，而不会将指定地址的零钱作为交易输入
    % 因为充值扫块还需要调用此接口，所以设为false，而不是unsupported
    false.

get_new_address() ->
    {ok, Addr} = rpc_request(getnewaddress, []),
    PrivKey = dump_priv_key(Addr),
    {Addr, PrivKey}.

dump_priv_key(Addr) ->
    {ok, PrivKey} = rpc_request(dumpprivkey, [Addr]),
    PrivKey.

% 注意，得到的是整个钱包内该币种的余额
get_balance(Symbol, _Addr) ->
    {ok, #{<<"confirmed">> := ConfirmedBalance}} = rpc_request(getbalance, [asset_id(Symbol)]),
    util:binary_to_float(ConfirmedBalance).

make_tx(Symbol, ToAddr, [{_FromAddr, Amount}], Gas) ->
    % 和文档里描述不一样，转账金额可以不是整数
    {ok, #{<<"txid">> := TxId}} = rpc_request(sendtoaddress, [asset_id(Symbol), ToAddr, util:round10d(Amount - Gas)]),
    % Neo链在这里已签过名，不需要再调用sign_tx，并且也已经send_tx
    TxId.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx) ->
    SignedTx.

get_latest_block_id() ->
    {ok, Hash} = rpc_request(getbestblockhash, []),
    Hash.

get_block(BlockId) ->
    {ok, #{<<"confirmations">> := Confirmations, <<"index">> := Height, <<"tx">> := TxList} = ResMap} =
        rpc_request(getblock, [BlockId, 1]),
    NextBlockId = case maps:is_key(<<"nextblockhash">>, ResMap) of
                      true -> #{<<"nextblockhash">> := NBId} = ResMap, NBId;
                      false -> undefined
                  end,
    PrevBlockId = case maps:is_key(<<"previousblockhash">>, ResMap) of
                      true -> #{<<"previousblockhash">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    TxIds = [TxId || #{<<"txid">> := TxId} <- TxList],
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

% SYMBOL_NEO 转账没有链上的手续费
get_tx(TxId) ->
    case rpc_request(getrawtransaction, [TxId, 1]) of
        {ok, #{<<"type">> := <<"ContractTransaction">>, <<"vin">> := Vin, <<"vout">> := Vout, <<"confirmations">> := Confirmations}} ->
            case get_tx_symbol(Vout) of
                unsupported_symbol ->
                    symbol_we_dont_care;
                Symbol ->
                    InAddrValPairs0 =
                        [begin
                             {ok, #{<<"vout">> := InVout}} = rpc_request(getrawtransaction, [InTxId, 1]),
                             case [{Addr, util:binary_to_float(Val)} || #{<<"address">> := Addr, <<"value">> := Val, <<"n">> := N} <- InVout, N =:= InVoutIdx] of
                                 [{InAddr, InVal}] -> {InAddr, InVal};
                                 [] -> void
                             end
                         end || #{<<"txid">> := InTxId, <<"vout">> := InVoutIdx} <- Vin],
                    InAddrValPairs =
                        lists:foldl(fun({InAddr, InVal}, Acc) ->
                                        case lists:keyfind(InAddr, 1, Acc) of
                                            false -> [{InAddr, InVal} | Acc];
                                            {_, InValAcc} -> lists:keyreplace(InAddr, 1, Acc, {InAddr, InValAcc + InVal})
                                        end;
                                       (_, Acc) -> Acc
                                    end, [], InAddrValPairs0),
                    {InAddrList, InValList} = lists:unzip(InAddrValPairs),
                    InTotalVal = lists:sum(InValList),
                    OutAddrValPairs0 = [{Addr, util:binary_to_float(Val)} || #{<<"address">> := Addr, <<"value">> := Val} <- Vout],
                    {OutAddrValPairs, Change} =
                        lists:foldl(fun({OutAddr, OutVal}, {OutAddrValPairsAcc, ChangeAcc}) ->
                                        case lists:member(OutAddr, InAddrList) of
                                            true -> {OutAddrValPairsAcc, ChangeAcc + OutVal};
                                            false ->
                                                case lists:keyfind(OutAddr, 1, OutAddrValPairsAcc) of
                                                    false -> {[{OutAddr, OutVal} | OutAddrValPairsAcc], ChangeAcc};
                                                    {_, OutValAcc} -> {lists:keyreplace(OutAddr, 1, OutAddrValPairsAcc, {OutAddr, OutValAcc + OutVal}), ChangeAcc}
                                                end
                                        end
                                    end, {[], 0}, OutAddrValPairs0),
                    {OutAddrList, OutValList} = lists:unzip(OutAddrValPairs),
                    OutTotalVal = lists:sum(OutValList),
                    Gas = util:round10d((InTotalVal * ?FACTOR - OutTotalVal * ?FACTOR - Change * ?FACTOR) / ?FACTOR),
                    {InAddrList, OutAddrList, Symbol, OutValList, Gas, Confirmations}
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> temporarily_not_found;
                false -> tx_type_we_dont_care
            end
    end.

get_tx_symbol(Vout) ->
    Assets = [Asset || #{<<"asset">> := Asset} <- Vout],
    case lists:member(<<"0x", (?ASSETID_NEO)/binary>>, Assets) of
        true -> ?SYMBOL_NEO;
        false ->
            GasAssetId = <<"0x", (?ASSETID_GAS)/binary>>,
            case lists:usort(Assets) of
                [GasAssetId] -> ?SYMBOL_GAS;
                _ -> unsupported_symbol
            end
    end.

import_priv_key(_PrivKey) ->
    unsupported.

rpc_request(M, Args) ->
    case lib_chain:rpc_request(?CHAIN, M, Args) of
        {error, #{<<"code">> := -400}} -> % Access denied
            ?ERR("Please open wallet manually !!!", []);
        Res -> Res
    end.

asset_id(?SYMBOL_NEO) -> ?ASSETID_NEO;
asset_id(?SYMBOL_GAS) -> ?ASSETID_GAS;
asset_id(_Symbol) -> <<>>.

