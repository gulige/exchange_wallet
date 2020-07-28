%%%------------------------------------
%%% @Module  : exchange_wallet_confirm
%%% @Description: 钱包确认进程
%%%------------------------------------
-module(exchange_wallet_confirm).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("exchange_common/include/common.hrl").

-define(PROCESS_INTERVAL, 10 * 1000).

-record(status, {
    chain = <<>>
}).

start_link(Chain) ->
    gen_server:start_link({local, binary_to_atom(<<"exchange_wallet_confirm_worker_", Chain/binary>>, utf8)}, ?MODULE, [Chain], []).

init([Chain]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?PROCESS_INTERVAL + util:rand(0, ?PROCESS_INTERVAL), self(), to_process),
    {ok, #status{chain = Chain}}.

handle_cast(_R, Status) ->
    {noreply, Status}.

handle_call(_R, _FROM, Status) ->
    {reply, ok, Status}.

handle_info(to_process, #status{chain = Chain} = Status) ->
    catch lib_confirm:do_chain(Chain),
    erlang:send_after(?PROCESS_INTERVAL, self(), to_process),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

