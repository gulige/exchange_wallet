%%%--------------------------------------
%%% @Module  : lib_tx_store
%%% @Description:tx存储
%%%--------------------------------------
-module(lib_tx_store).

-include_lib("exchange_common/include/common.hrl").

-export([init/1,
         does_trans_id_exist/2,
         get_tx/2,
         new_tx/3,
         new_tx/4,
         upd_tx/2,
         get_total_withdrawing_balance/3,
         get_todo_tx_list/1]).

-define(TXSTORE, <<"txstore">>).

init(Chains) ->
    [begin
         Table = table_name(Chain),
         TableBin = atom_to_binary(Table, utf8),
         ets:new(Table, [named_table, public, set, ?ETSRC, ?ETSWC]),
         lib_mongo:ensure_index(Chain, TableBin, #{<<"key">> => {<<"trans_id">>, 1}}),
         lib_mongo:ensure_index(Chain, TableBin, #{<<"key">> => {<<"from_addr">>, 1, <<"is_withdraw">>, 1, <<"symbol">>, 1, <<"confirm">>, 1}}),
         lib_mongo:ensure_index(Chain, TableBin, #{<<"key">> => {<<"notify_finance">>, 1}})
     end || Chain <- Chains],
    ok.

does_trans_id_exist(Chain, TransId) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    case lib_mongo:find_one(Chain, TableBin, #{<<"is_invalid">> => 0, <<"trans_id">> => TransId}, #{<<"_id">> => true}) of
        undefined -> false;
        _ -> true
    end.

% 没查到，返回的是undefined
get_tx(Chain, TxId) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    case ets:lookup(Table, TxId) of
        [] ->
            case lib_mongo:find_one(Chain, TableBin, #{<<"_id">> => TxId}, #{}) of
                TxR when is_map(TxR) ->
                    ets:insert(Table, {TxId, TxR}),
                    TxR;
                Res ->
                    Res
            end;
        [{_, TxR}] ->
            TxR
    end.

new_tx(Chain, TxId, TxParams) ->
    new_tx(Chain, TxId, TxParams, 0).

new_tx(Chain, TxId, [TransId, IsWithdraw, FromAddrList, ToAddr, Symbol, Amount, Fee, Gas, WithdrawDeltaAmountList], IsInternal) ->
    TxR = #{<<"_id">> => TxId,
            <<"trans_id">> => TransId,
            <<"is_withdraw">> => IsWithdraw,
            <<"from_addr">> => FromAddrList,
            <<"to_addr">> => ToAddr,
            <<"symbol">> => Symbol,
            <<"amount">> => Amount,
            <<"withdraw_delta_amounts">> => WithdrawDeltaAmountList,
            <<"confirm">> => 0,
            <<"real_fee">> => util:round10d(Fee - Gas),
            <<"notify_finance">> => 0,
            <<"time">> => util:unixtime(),
            <<"is_internal">> => IsInternal,
            <<"is_invalid">> => 0},
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    ets:insert(Table, {TxId, TxR}),
    {{true, _}, _} = lib_mongo:write_one(Chain, TableBin, TxR),
    ok.

upd_tx(Chain, #{<<"_id">> := TxId} = TxR0) ->
    TxR = TxR0#{<<"time">> := util:unixtime()},
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    ets:insert(Table, {TxId, TxR}),
    {true, _} = lib_mongo:write_one(Chain, TableBin, #{<<"_id">> => TxId}, TxR),
    ok.

get_total_withdrawing_balance(Chain, Symbol, Addr) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    NeedConfirmCount = lib_chain_cfg:confirm_count(Chain),
    AddrAmountList = lib_mongo:find_all(Chain, TableBin,
                                        #{<<"is_invalid">> => 0, <<"is_withdraw">> => 1, <<"from_addr">> => Addr, <<"symbol">> => Symbol,
                                          <<"confirm">> => #{<<"$lt">> => NeedConfirmCount}},
                                        #{<<"_id">> => false, <<"from_addr">> => true, <<"withdraw_delta_amounts">> => true}),
    Amounts = [begin
                   AddrDeltaAmountPairs = lists:zip(Addrs, DeltaAmounts),
                   {_, DeltaAmount} = lists:keyfind(Addr, 1, AddrDeltaAmountPairs),
                   DeltaAmount
               end || #{<<"from_addr">> := Addrs, <<"withdraw_delta_amounts">> := DeltaAmounts} <- AddrAmountList],
    lists:sum(Amounts).

get_todo_tx_list(Chain) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    lib_mongo:find_all(Chain, TableBin, #{<<"is_invalid">> => 0, <<"notify_finance">> => 0}, #{}).

table_name(Chain) when is_binary(Chain) ->
    binary_to_atom(<<(?TXSTORE)/binary, "_", Chain/binary>>, utf8).

