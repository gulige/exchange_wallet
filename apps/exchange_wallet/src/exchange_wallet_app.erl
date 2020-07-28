%%%-------------------------------------------------------------------
%% @doc exchange_wallet public API
%% @end
%%%-------------------------------------------------------------------

-module(exchange_wallet_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("exchange_common/include/common.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = exchange_wallet_sup:start_link(),
    % 钱包节点只承载所指定的链的充值、提取
    {ok, ChainsDespList} = application:get_env(exchange_common, chains),
    SelfServerId = mod_disperse:server_id(),
    Chains = lists:usort([atom_to_binary(Chain, utf8) || {Chain, ServerId} <- ChainsDespList,
                                                         ServerId =:= SelfServerId]),
    try
        ok = start_mongo(Chains),
        ok = start_disperse(),
        ok = lib_block_scan_mark:init(Chains),
        ok = lib_chain_addrs:init(Chains),
        ok = lib_chain_balances:init(Chains),
        ok = lib_tx_store:init(Chains),
        ok = start_deposit_workers(Chains),
        ok = start_withdraw_workers(Chains),
        ok = start_confirm_workers(Chains)
    catch
        _:Err ->
            ?ERR("node starting error=~p, stack=~p~n", [Err, erlang:get_stacktrace()]),
            throw(node_starting_error)
    end,
    ?INFO("server ready.~n", []),
    {ok, SupPid}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%%开启服务器自动发现
start_disperse() ->
    ?INFO("disperse starting...~n", []),
    {ok, _} = supervisor:start_child(
                exchange_wallet_sup,
                {mod_disperse,
                {mod_disperse, start_link, [?SVRTYPE_WALLET_NODE]},
                permanent, 10000, supervisor, [mod_disperse]}),
    ?INFO("disperse started~n", []),
    ok.

start_mongo(Chains) ->
    ?INFO("mongo starting...~n", []),
    {ok, _} = application:ensure_all_started(mongodb),
    lib_mongo:start_mongo_pools(Chains),
    ?INFO("mongo started~n", []),
    ok.

start_deposit_workers(Chains) ->
    {ok, _} = supervisor:start_child(
                exchange_wallet_sup,
                {exchange_wallet_deposit_sup,
                {exchange_wallet_deposit_sup, start_link, []},
                permanent, 10000, supervisor, [exchange_wallet_deposit_sup]}),
    ?INFO("deposit workers starting...~n", []),
    [start_one_deposit_worker(Chain) || Chain <- Chains],
    ok.

start_withdraw_workers(Chains) ->
    {ok, _} = supervisor:start_child(
                exchange_wallet_sup,
                {exchange_wallet_withdraw_sup,
                {exchange_wallet_withdraw_sup, start_link, []},
                permanent, 10000, supervisor, [exchange_wallet_withdraw_sup]}),
    ?INFO("withdraw workers starting...~n", []),
    [start_one_withdraw_worker(Chain) || Chain <- Chains],
    ok.

start_confirm_workers(Chains) ->
    {ok, _} = supervisor:start_child(
                exchange_wallet_sup,
                {exchange_wallet_confirm_sup,
                {exchange_wallet_confirm_sup, start_link, []},
                permanent, 10000, supervisor, [exchange_wallet_confirm_sup]}),
    ?INFO("confirm workers starting...~n", []),
    [start_one_confirm_worker(Chain) || Chain <- Chains],
    ok.

start_one_deposit_worker(Chain) ->
    {ok, _} = supervisor:start_child(exchange_wallet_deposit_sup, [Chain]),
    ?INFO("deposit worker started for chain: ~s~n", [Chain]),
    ok.

start_one_withdraw_worker(Chain) ->
    {ok, _} = supervisor:start_child(exchange_wallet_withdraw_sup, [Chain]),
    ?INFO("withdraw worker started for chain: ~s~n", [Chain]),
    ok.

start_one_confirm_worker(Chain) ->
    {ok, _} = supervisor:start_child(exchange_wallet_confirm_sup, [Chain]),
    ?INFO("confirm worker started for chain: ~s~n", [Chain]),
    ok.

