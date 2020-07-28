%%%-----------------------------------
%%% @Module  : lib_rpc
%%% @Description: rpc库模块
%%%-----------------------------------
-module(lib_rpc).
-export([rpc_sign_node/4]).
-export([find_one_sign_node/1]).

-include_lib("exchange_common/include/common.hrl").

rpc_sign_node(Chain, M, F, A) ->
    SignNode = find_one_sign_node(Chain),
    case rpc:call(SignNode, M, F, A) of
        {badrpc, Reason} ->
            ?ERR("rpc exception:~nerr_msg=~p~n", [Reason]),
            throw({error, ?T2B(Reason)});
        {error, ErrMsg} ->
            throw({error, ErrMsg});
        Ret -> Ret
    end.

find_one_sign_node(TheChain0) ->
    % 签名节点只承载所指定的链的签名，所以需要筛选
    TheChain = binary_to_atom(TheChain0, utf8),
    {ok, ChainsDespList} = application:get_env(exchange_common, chains),
    ChainServerIds = lists:usort([ServerId || {Chain, ServerId} <- ChainsDespList, Chain =:= TheChain]),
    SignNodes = [Node || {ServerId, Node} <- mod_disperse:sign_svrid_node_pairs(), lists:member(ServerId, ChainServerIds)],
    case SignNodes of
        [] -> throw({error, <<"no sign node found">>});
        [SoleNode] -> SoleNode;
        _ -> lists:nth(util:rand(1, length(SignNodes)), SignNodes)
    end.
