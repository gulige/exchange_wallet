%%%--------------------------------------
%%% @Module  : chain_ttt
%%% @Description: TrustNote链的访问接口
%%%--------------------------------------
-module(chain_ttt).

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
-export([get_all_deposit_txs_from_outside_to_my_wallet/1]).

-include("common.hrl").

-define(CHAIN, <<"ttt">>).
-define(SYMBOL, <<"ttt">>).
-define(FACTOR, 1000000).


start_node() ->
    ?INFO("starting ttt node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/ttt; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/ttt/stop.sh"),
    ok.

need_gather() ->
    % TrustNote内部管理余额，转账时不需要提供From地址，因此不需要维护余额，也不需要汇总
    unsupported.

get_new_address() ->
    {ok, Addr} = lib_chain:rpc_request(?CHAIN, getnewaddress),
    {Addr, <<>>}.

get_balance(?SYMBOL, Addr) ->
    {ok, #{<<"base">> := #{<<"stable">> := Balance}}} =
        lib_chain:rpc_request(?CHAIN, getbalance, [Addr]),
    Balance;
get_balance(_Symbol, _Addr) ->
    0.

make_tx(?SYMBOL, ToAddr, [{_FromAddr, Amount}], Gas) ->
    case lib_chain:rpc_request(?CHAIN, sendtoaddress, [ToAddr, trunc(util:round10d(Amount - Gas) * ?FACTOR)]) of
        {ok, TxId} ->
            % TrustNote链在这里已签过名，不需要再调用sign_tx，并且也已经广播
            TxId
    end;
make_tx(_Symbol, _ToAddr, _AddrToDeltaAmountPairs, _Gas) ->
    <<>>.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx) ->
    SignedTx.

get_latest_block_id() ->
    {ok, #{<<"last_mci">> := LastMCI}} = lib_chain:rpc_request(?CHAIN, getinfo, []),
    LastMCI.

get_block(_BlockId) ->
    unsupported.

get_tx(TxId) ->
    {ok, [TheTx | _] = TxList} = lib_chain:rpc_request(?CHAIN, listtransactions, [{unit, TxId}]),
    #{<<"action">> := Action,
      <<"confirmations">> := Confirmations, % 0 - pending, 1 - final
      <<"amount">> := Amount,
      <<"fee">> := Gas} = TheTx,
    case Action of
        <<"received">> ->
            #{<<"arrPayerAddresses">> := FromAddrList, <<"my_address">> := ToAddr} = TheTx,
            {FromAddrList, [ToAddr], ?SYMBOL, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
        <<"sent">> ->
            #{<<"addressTo">> := ToAddr} = TheTx,
            {[<<>>], [ToAddr], ?SYMBOL, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
        <<"moved">> ->
            % 排除找零地址，找零地址总是新建的，我们不关心
            [TheTx2] = [Tx || (#{<<"addressTo">> := ToAddr} = Tx) <- TxList, lib_chain_addrs:is_addr_ours(?CHAIN, ToAddr)],
            #{<<"confirmations">> := Confirmations2,
              <<"amount">> := Amount2,
              <<"fee">> := Gas2,
              <<"addressTo">> := ToAddr2} = TheTx2,
            {[<<>>], [ToAddr2], ?SYMBOL, [Amount2 / ?FACTOR], Gas2 / ?FACTOR, Confirmations2};
        _ ->
            tx_type_we_dont_care
    end.

% 返回{TxId, FromAddrList, ToAddr, Symbol, Amount, Gas, Level, MCI}的列表，先按MCI排序，MCI相同则按时间先后排序
% 注意，同一个mci中交易的confirmations，要么都是1，要么都是0
get_all_deposit_txs_from_outside_to_my_wallet(SinceMCI) ->
    {ok, TxList} = lib_chain:rpc_request(?CHAIN, listtransactions, [{since_mci, SinceMCI}]),
    L = [case Action of
             <<"received">> -> % 从钱包外地址打进来的
                 #{<<"arrPayerAddresses">> := FromAddrList, <<"my_address">> := ToAddr} = Tx,
                 {{MCI, Time}, {TxId, FromAddrList, ToAddr, ?SYMBOL, Amount / ?FACTOR, Gas / ?FACTOR, Level, MCI}};
             <<"moved">> -> % 从钱包内地址挪过来的
                 #{<<"addressTo">> := ToAddr} = Tx,
                 case lib_chain_addrs:is_addr_ours(?CHAIN, ToAddr) of
                     true ->
                         {{MCI, Time}, {TxId, [<<>>], ToAddr, ?SYMBOL, Amount / ?FACTOR, Gas / ?FACTOR, Level, MCI}};
                     false -> % 找零地址，找零地址总是新建的，我们不关心
                         void
                 end
         end || (#{<<"action">> := Action,
                   <<"confirmations">> := 1, % 0 - pending, 1 - final
                   <<"unit">> := TxId,
                   <<"amount">> := Amount,
                   <<"fee">> := Gas,
                   <<"level">> := Level,
                   <<"mci">> := MCI,
                   <<"time">> := Time} = Tx) <- TxList, Action =:= <<"received">> orelse Action =:= <<"moved">>],
    SortedL = lists:keysort(1, [One || One <- L, One =/= void]),
    [Tx || {_, Tx} <- SortedL].

import_priv_key(_PrivKey) ->
    unsupported.

