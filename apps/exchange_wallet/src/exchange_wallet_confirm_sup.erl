%%%-------------------------------------------------------------------
%% @doc exchange_wallet confirm supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(exchange_wallet_confirm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {simple_one_for_one, 3, 10},
           [{exchange_wallet_confirm,
             {exchange_wallet_confirm, start_link, []},
             permanent,
             10000,
             worker,
             [exchange_wallet_confirm]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
