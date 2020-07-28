%%%--------------------------------------
%%% @Module  : lib_chain_addrs
%%% @Description:某种链的地址集合
%%%--------------------------------------
-module(lib_chain_addrs).

-include_lib("exchange_common/include/common.hrl").

-export([init/1,
         load_addrs/1,
         is_addr_ours/2,
         is_addr_ours/3,
         traverse/3,
         get_addrs_by_type/2,
         dump_addrs/2]).

-define(ADDRESS, <<"address">>).

init(Chains) ->
    [begin
         ets:new(table_name(Chain), [named_table, public, set, ?ETSRC, ?ETSWC]),
         load_addrs(Chain)
     end || Chain <- Chains],
    ok.

load_addrs(Chain) ->
    F = fun(#{<<"_id">> := Addr, <<"addr_type">> := AddrType}) ->
            Table = table_name(Chain),
            ets:insert(Table, {Addr, AddrType}),
            case AddrType of
                ?ADDR_TYPE_NORMAL -> void;
                _ ->
                    case ets:lookup(Table, AddrType) of
                        [] -> ets:insert(Table, {AddrType, [Addr]});
                        [{_, Addrs}] ->
                            case lists:member(Addr, Addrs) of
                                true -> void;
                                false -> ets:insert(Table, {AddrType, [Addr | Addrs]})
                            end
                    end
            end
        end,
    lib_mongo:iterate(Chain, <<"address">>, #{}, #{<<"_id">> => true, <<"addr_type">> => true}, F),
    ok.

is_addr_ours(Chain, Addr) ->
    Table = table_name(Chain),
    case ets:lookup(Table, Addr) of
        [{_, _}] -> true;
        _ -> false
    end.

is_addr_ours(Chain, Addr, AddrType) ->
    Table = table_name(Chain),
    case ets:lookup(Table, Addr) of
        [{_, AddrType}] -> true;
        _ -> false
    end.

get_addrs_by_type(Chain, AddrType) ->
    Table = table_name(Chain),
    case ets:lookup(Table, AddrType) of
        [] -> [];
        [{_, Addrs}] -> Addrs
    end.

dump_addrs(Chain, AddrType) ->
    {ok, File} = file:open(binary_to_list(Chain) ++ "_addresses_" ++ integer_to_list(AddrType), write),
    F = fun(#{<<"_id">> := Addr}) ->
            io:format(File, "~s~n", [Addr])
        end,
    lib_mongo:iterate(Chain, <<"address">>, #{<<"addr_type">> => AddrType}, #{<<"_id">> => true}, F),
    file:close(File),
    ok.

traverse(Chain, AddrType, F) when is_function(F, 1),
                                  (AddrType =:= ?ADDR_TYPE_GATHER) ->
    Addrs = get_addrs_by_type(Chain, AddrType),
    [F(Addr) || Addr <- Addrs],
    ok;
traverse(Chain, NeedAddrType, F) when is_function(F, 1) ->
    WrapF = fun(_I, {Addr, AddrType}) when is_binary(Addr) ->
                case NeedAddrType =:= ?ADDR_TYPE_ALL orelse AddrType =:= NeedAddrType of
                    true -> F(Addr);
                    false -> void
                end;
               (_, _) -> void
            end,
    Table = table_name(Chain),
    ets:safe_fixtable(Table, true),
    util:traverse_ets(WrapF, none, Table),
    ets:safe_fixtable(Table, false),
    ok.

table_name(Chain) when is_binary(Chain) ->
    binary_to_atom(<<(?ADDRESS)/binary, "_", Chain/binary>>, utf8).

