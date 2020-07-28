%%%--------------------------------------
%%% @Module  : lib_mongo
%%% @Description: mongodb的访问接口
%%% 说明：每条链一个库，而common是在链外的库
%%%--------------------------------------
-module(lib_mongo).

-export([start_mongo_pools/1, start_mongo_pool/1]).
-export([write_one/3, write_one/4, find_one/4, find_all/4, iterate/5, ensure_index/3]).
-export([write_common_one/2, write_common_one/3, find_common_one/3, find_common_all/3, iterate_common/4]).

-include("common.hrl").

-define(ETS_MONGOPOOL, ets_mongo_pool).
-define(COMMON_POOL, <<"common">>).

start_mongo_pools(Chains) ->
    ets:new(?ETS_MONGOPOOL, [named_table, public, set, ?ETSRC]),
    start_mongo_pool(?COMMON_POOL),
    [start_mongo_pool(Chain) || Chain <- Chains].

% 可以直接调用，动态启动一条新链的mongo pool
start_mongo_pool(Chain) when is_binary(Chain) ->
    {ok, {Hosts, PoolArgs, MgArgs0}} = application:get_env(exchange_common, mg_pool),
    {_, DbName0} = lists:keyfind(database, 1, MgArgs0),
    DbName = <<DbName0/binary, "_", Chain/binary>>,
    MgArgs = lists:keyreplace(database, 1, MgArgs0, {database, DbName}),
    {ok, Pid} = mongo_api:connect(single, Hosts, PoolArgs, MgArgs),
    ets:insert(?ETS_MONGOPOOL, {Chain, Pid}),
    ?INFO("mongo pool started: hosts=~p, poolargs=~p, mgargs=~p~n", [Hosts, PoolArgs, MgArgs]),
    ok.

ensure_index(Chain, Table, IndexOpts) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] ->
            ok = mongo_api:ensure_index(Pid, Table, IndexOpts)
    end.

write_one(Chain, Table, One) when is_binary(Chain), is_binary(Table), is_map(One) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] ->
            mongo_api:insert(Pid, Table, [One])
    end.

write_one(Chain, Table, Selector, UpdateMap) when is_binary(Chain), is_binary(Table), is_map(Selector), is_map(UpdateMap) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] ->
            mongo_api:update(Pid, Table, Selector, #{<<"$set">> => bson:flatten_map(UpdateMap)}, #{upsert => true})
    end.

find_one(Chain, Table, Selector, Projector) when is_binary(Chain), is_binary(Table), is_map(Selector), is_map(Projector) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] -> mongo_api:find_one(Pid, Table, Selector, Projector)
    end.

find_all(Chain, Table, Selector, Projector) when is_binary(Chain), is_binary(Table), is_map(Selector), is_map(Projector) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] ->
            case mongo_api:find(Pid, Table, Selector, Projector) of
                {ok, Cursor} ->
                    DataList = mc_cursor:rest(Cursor),
                    mc_cursor:close(Cursor),
                    DataList;
                _ -> []
            end
    end.

iterate(Chain, Table, Selector, Projector, Fun) when is_binary(Chain), is_binary(Table), is_map(Selector), is_map(Projector), is_function(Fun, 1) ->
    case ets:lookup(?ETS_MONGOPOOL, Chain) of
        [] -> throw({error, <<"no mongo pool found for chain=", Chain/binary>>});
        [{_, Pid}] ->
            case mongo_api:find(Pid, Table, Selector, Projector) of
                {ok, Cursor} ->
                    IterF =
                        fun(IterF) ->
                            case mc_cursor:next(Cursor, infinity) of
                                error -> error;
                                {} -> done;
                                {Doc} ->
                                    Fun(Doc),
                                    IterF(IterF)
                            end
                        end,
                    IterF(IterF),
                    mc_cursor:close(Cursor);
                _ -> void
            end
    end.

write_common_one(Table, One) ->
    write_one(?COMMON_POOL, Table, One).

write_common_one(Table, Selector, UpdateMap) ->
    write_one(?COMMON_POOL, Table, Selector, UpdateMap).

find_common_one(Table, Selector, Projector) ->
    find_one(?COMMON_POOL, Table, Selector, Projector).

find_common_all(Table, Selector, Projector) ->
    find_all(?COMMON_POOL, Table, Selector, Projector).

iterate_common(Table, Selector, Projector, Fun) ->
    iterate(?COMMON_POOL, Table, Selector, Projector, Fun).

