%%%-------------------------------------------------------------------
%% @doc exchange_sign public API
%% @end
%%%-------------------------------------------------------------------

-module(exchange_sign_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("exchange_common/include/common.hrl").

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = exchange_sign_sup:start_link(),
    % 签名节点只承载所指定的链的签名
    {ok, ChainsDespList} = application:get_env(exchange_common, chains),
    SelfServerId = mod_disperse:server_id(),
    Chains = lists:usort([atom_to_binary(Chain, utf8) || {Chain, ServerId} <- ChainsDespList,
                                                         ServerId =:= SelfServerId]),
    ok = start_mongo(Chains),
    ok = start_disperse(),
    ok = lib_keystore:init(Chains),
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
    {ok,_} = supervisor:start_child(
                exchange_sign_sup,
                {mod_disperse,
                {mod_disperse, start_link, [?SVRTYPE_SIGN_NODE]},
                permanent, 10000, supervisor, [mod_disperse]}),
    ?INFO("disperse started~n", []),
    ok.

start_mongo(Chains) ->
    ?INFO("mongo starting...~n", []),
    {ok, _} = application:ensure_all_started(mongodb),
    lib_mongo:start_mongo_pools(Chains),
    lib_mongo_priv:start_mongo_pools(Chains),
    ?INFO("mongo started~n", []),
    ok.

