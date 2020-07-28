%%%--------------------------------------
%%% @Module  : lib_block_scan_mark
%%% @Description:块扫描标记
%%%--------------------------------------
-module(lib_block_scan_mark).

-include_lib("exchange_common/include/common.hrl").

-export([init/1,
         get_scan_mark/1,
         set_scan_mark/4]).

-define(BLOCK_SCAN, block_scan).
-define(BLOCK_SCAN_START, block_scan_start).

init(Chains) ->
    ets:new(?BLOCK_SCAN, [named_table, public, set, ?ETSRC, ?ETSWC]),
    [load_scan_mark(Chain) || Chain <- Chains],
    % 必须要等到链节点确保包含了上次扫描标记的块，才能保证后面余额查询正确
    [wait_chain_node_sync_to_scan_mark(Chain) || Chain <- Chains],
    ok.

load_scan_mark(Chain) ->
    case lib_mongo:find_common_one(atom_to_binary(?BLOCK_SCAN, utf8), #{<<"_id">> => Chain}, #{<<"block_id">> => true, <<"done">> => true, <<"time">> => true}) of
        #{<<"block_id">> := BlockId, <<"done">> := Done, <<"time">> := Time} ->
            ets:insert(?BLOCK_SCAN, {Chain, BlockId, Done, Time}),
            {BlockId, Done};
        _ ->
            Now = util:unixtime(),
            % 尚未建立标记时，第一次从当前高度开始
            BlockId = lib_chain:get_latest_block_id(Chain),
            ets:insert(?BLOCK_SCAN, {Chain, BlockId, 0, Now}),
            % 记录起始块和起始时间戳
            {true, _} = lib_mongo:write_common_one(atom_to_binary(?BLOCK_SCAN_START, utf8), #{<<"_id">> => Chain}, #{<<"block_id">> => BlockId, <<"time">> => Now}),
            {BlockId, 0}
    end.

wait_chain_node_sync_to_scan_mark(<<"ttt">> = Chain) ->
    {BlockId, _} = get_scan_mark(Chain),
    ?INFO("waiting chain(~s) node sync to scan mark: ~p...~n", [Chain, BlockId]),
    LatestBlockId = lib_chain:get_latest_block_id(Chain),
    case LatestBlockId >= BlockId of
        true ->
            ok;
        _ ->
            timer:sleep(5000),
            wait_chain_node_sync_to_scan_mark(Chain)
    end;
wait_chain_node_sync_to_scan_mark(Chain) ->
    {BlockId, _} = get_scan_mark(Chain),
    ?INFO("waiting chain(~s) node sync to scan mark: ~p...~n", [Chain, BlockId]),
    case catch lib_chain:get_block(Chain, BlockId) of
        {_Height, _TxIds, _PrevBlockId, _NextBlockId, _Confirmations} ->
            ok;
        _ ->
            timer:sleep(5000),
            wait_chain_node_sync_to_scan_mark(Chain)
    end.

get_scan_mark(Chain) ->
    case ets:lookup(?BLOCK_SCAN, Chain) of
        [] -> load_scan_mark(Chain);
        [{_Chain, BlockId, Done, _Time}] -> {BlockId, Done}
    end.

% 不要手动设置扫描标记
set_scan_mark(Chain, BlockId, Height, Done) ->
    Now = util:unixtime(),
    ets:insert(?BLOCK_SCAN, {Chain, BlockId, Done, Now}),
    {true, _} = lib_mongo:write_common_one(atom_to_binary(?BLOCK_SCAN, utf8), #{<<"_id">> => Chain}, #{<<"block_id">> => BlockId, <<"height">> => Height, <<"done">> => Done, <<"time">> => Now}),
    ok.

