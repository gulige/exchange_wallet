%%%-------------------------------------------------------------------
%% @doc exchange_wallet deposit supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(exchange_wallet_deposit_sup).

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
           [{exchange_wallet_deposit,
             {exchange_wallet_deposit, start_link, []},
             permanent,
             10000,
             worker,
             [exchange_wallet_deposit]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
