%%%------------------------------------
%%% @Module  : mod_disperse
%%% @Description: 服务器自动发现
%%%------------------------------------
-module(mod_disperse).
-behaviour(gen_server).

-export([start_link/1,
         rpc_server_add/3,
         server_id/0,
         server_type/0,
         sign_nodes/0,
         sign_svrid_node_pairs/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(WAIT_TIME, 10000).
-define(ETS_SIGN_NODES, ets_sign_nodes).
-define(MONGO_SERVER_TABLE, <<"server">>).

-record(server, {
        id,
        node,
        type
    }
).

%% 查询当前服务器id号
%% 返回:int()
server_id() ->
    Node = node(),
    {ok, MP} = re:compile("_[0-9]+@", []),
    {match, [S]} = re:run(atom_to_binary(Node, utf8), MP, [{capture, first, list}]),
    list_to_integer(lists:sublist(S, 2, length(S) - 2)).

server_type() ->
    server_id() div 10000.

sign_nodes() ->
    [Node || #server{node = Node} <- ets:tab2list(?ETS_SIGN_NODES)].

sign_svrid_node_pairs() ->
    [{SvrId, Node} || #server{id = SvrId, node = Node} <- ets:tab2list(?ETS_SIGN_NODES)].

%% 接收其它服务器的加入信息
rpc_server_add(Id, Node, Type) ->
    ?MODULE ! {rpc_server_add, Id, Node, Type}.

start_link(Type) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Type], []).

init([Type]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    ets:new(?ETS_SIGN_NODES, [{keypos, #server.id}, named_table, public, set, ?ETSRC, ?ETSWC]),
    State = #server{id = server_id(), node = node(), type = Type},
    add_server([State#server.id, State#server.node, State#server.type]),
    erlang:send_after(?WAIT_TIME, ?MODULE, get_and_call_server),
    {ok, State}.

handle_cast(_R , State) ->
    {noreply, State}.

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

%% 获取并通知当前所有服务器
handle_info(get_and_call_server, State) ->
    get_and_call_server(State),
    [erlang:monitor_node(Node, true) || Node <- sign_nodes()],
    {noreply, State};

%% 新服务器加入
handle_info({rpc_server_add, Id, Node, Type}, State) ->
    case Type of
        ?SVRTYPE_WALLET_NODE ->
            skip;
        ?SVRTYPE_SIGN_NODE ->
            case State#server.type =:= ?SVRTYPE_WALLET_NODE of
                true ->
                    ?INFO("sign node added: id=~p, node=~p, type=~p", [Id, Node, Type]),
                    erlang:monitor_node(Node, true),
                    ets:insert(?ETS_SIGN_NODES, #server{id = Id, node = Node, type = Type});
                false ->
                    skip
            end;
        _ ->
            ?ERR("Unknown server type: id=~p, node=~p, type=~p", [Id, Node, Type]),
            skip
    end,
    {noreply, State};

%% 处理新节点加入事件
handle_info({nodeup, Node}, State) ->
    try
        rpc:cast(Node, mod_disperse, rpc_server_add, [State#server.id, State#server.node, State#server.type])
    catch
        _:_ -> skip
    end,
    {noreply, State};

%% 处理节点关闭事件
handle_info({nodedown, Node}, State) ->
    case ets:match_object(?ETS_SIGN_NODES, #server{node = Node, _ = '_'}) of
        [S] ->
            ?INFO("sign node nodedown: node=~p", [Node]),
            del_server(S#server.id),
            ets:match_delete(?ETS_SIGN_NODES, #server{node = Node, _ = '_'});
        _ ->
            skip
    end,
    {noreply, State};

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_R, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ----------------------- 私有函数 ---------------------------------

%%加入服务器集群
add_server([ServerId, Node, Type]) ->
    lib_mongo:write_common_one(?MONGO_SERVER_TABLE, #{<<"_id">> => ServerId}, #{<<"node">> => Node, <<"type">> => Type, <<"state">> => 1}).

%%退出服务器集群
del_server(ServerId) ->
    lib_mongo:write_common_one(?MONGO_SERVER_TABLE, #{<<"_id">> => ServerId}, #{<<"state">> => 0}).

%%获取并通知所有服务器信息
get_and_call_server(#server{type = SelfType} = State) ->
    Servers =
        case SelfType of
            ?SVRTYPE_WALLET_NODE ->
                lib_mongo:find_common_all(?MONGO_SERVER_TABLE, #{<<"type">> => ?SVRTYPE_SIGN_NODE}, #{<<"_id">> => true, <<"node">> => true, <<"type">> => true});
            ?SVRTYPE_SIGN_NODE ->
                lib_mongo:find_common_all(?MONGO_SERVER_TABLE, #{<<"type">> => ?SVRTYPE_WALLET_NODE}, #{<<"_id">> => true, <<"node">> => true, <<"type">> => true})
        end,
    F = fun(#{<<"_id">> := Id, <<"node">> := Node, <<"type">> := Type}) ->
            case Id /= State#server.id of  % 自己不写入和不通知
                true ->
                    PongF = fun() ->
                                case Type of
                                    ?SVRTYPE_SIGN_NODE ->
                                        ?INFO("sign node added: id=~p, node=~p, type=~p", [Id, Node, Type]),
                                        ets:insert(?ETS_SIGN_NODES,
                                            #server{
                                                id = Id,
                                                node = Node,
                                                type = Type
                                            }
                                        );
                                    _ ->
                                        ok
                                end,
                                %% 通知已有的服务器加入当前服务器的节点
                                rpc:cast(Node, mod_disperse, rpc_server_add, [State#server.id, State#server.node, State#server.type])
                            end,
                    case net_adm:ping(Node) of
                        pong ->
                            PongF();
                        pang ->
                            ?INFO("pang! try again using self cookie to ping ~p...~n", [Node]),
                            erlang:set_cookie(Node, erlang:get_cookie()),
                            case net_adm:ping(Node) of
                                pong ->
                                    ?INFO("pong! connected.~n", []),
                                    PongF();
                                pang ->
                                    del_server(Id)
                            end
                    end;
                false ->
                    ok
            end
        end,
    [F(S) || S <- Servers].

