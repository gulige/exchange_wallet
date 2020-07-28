%%%--------------------------------------
%%% @Module  : lib_chain
%%% @Description: 链的访问接口
%%%--------------------------------------
-module(lib_chain).

-export([start_node/1,
         stop_node/1,
         need_gather/1,
         get_new_address/1,
         get_balance/3,
         make_tx/5,
         sign_tx/3,
         send_tx/2,
         get_latest_block_id/1,
         get_block/2,
         get_tx/2,
         import_priv_key/2]).

-export([rpc_request/2, rpc_request/3, rpc_request/4]).
-export([get_symbols_by_chain/1]).

-include("common.hrl").

-define(SEED_BYTES, 16).
-define(HTTP_REQUEST_TIMEOUT, 60000 * 10).
-define(HTTP_CONNECTION_TIMEOUT, 10000).

%% 启动链节点
start_node(Chain) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:start_node().

%% 停止链节点
stop_node(Chain) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:stop_node().

%% 是否采用余额汇总流程
need_gather(Chain) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:need_gather().

%% 获取新地址，返回{地址, 私钥}
get_new_address(Chain) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:get_new_address().

%% 获取地址余额
get_balance(Chain, Symbol, Addr) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    util:round10d(M:get_balance(Symbol, Addr)).

%% 生成交易内容（允许多个输入地址，但如果采用余额汇总流程，则必须只是一个输入地址）
%% 注意，AddrToDeltaAmountPairs里的Amount包含了Gas
make_tx(Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    Tx = M:make_tx(Symbol, ToAddr, AddrToDeltaAmountPairs, Gas),
    true = is_binary(Tx) andalso Tx =/= <<>>, % 断言
    Tx.

%% 签名交易
sign_tx(Chain, Addr, Tx) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:sign_tx(Addr, Tx).

%% 发送交易
send_tx(Chain, SignedTx) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:send_tx(SignedTx).

%% 获取最新的块id（块哈希）
get_latest_block_id(Chain) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:get_latest_block_id().

%% 获取指定id的块信息
%% 返回：{Height, TxIds, PrevBlockId, NextBlockId, Confirmations}
%% 注意：如果还没有下一块，则NextBlockId = undefined
get_block(Chain, BlockId) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:get_block(BlockId).

%% 获取交易信息
%% 返回：{FromAddrList, ToAddrList, Symbol, AmountList, Gas, Confirmations}，不存在返回：undefined（临时分叉的情况下）
%% 注意：FromAddrList、ToAddrList需要去重排序，AmountList顺序与ToAddrList一致
get_tx(Chain, TxId) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:get_tx(TxId).

%% 向钱包导入私钥
import_priv_key(Chain, PrivKey) ->
    M = list_to_atom("chain_" ++ binary_to_list(Chain)),
    M:import_priv_key(PrivKey).

%% 向链节点发起jsonrpc请求
rpc_request(Chain, Request) ->
    rpc_request(Chain, Request, []).

rpc_request(Chain, Request, Params0) ->
    rpc_request(Chain, Request, Params0, false).

rpc_request(Chain, Request, Params0, IsPub) ->
    Method = atom_to_binary(Request, utf8),
    Seed = get_seed(),
    Params = case Params0 of
                 [{_, _} | _] -> {Params0};
                 Params0 -> Params0
             end,
    JsonReq = jsonrpc2_client:create_request(
        {Method, Params, base64:encode(Seed)}),
    do_jsonrpc_request(Chain, JsonReq, IsPub).

do_jsonrpc_request(Chain, JsonReq, IsPub) ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    JsonRpcCfgTag = binary_to_atom(<<"jsonrpc_", Chain/binary>>, utf8),
    {ok, CfgList} = application:get_env(App, JsonRpcCfgTag),
    case IsPub of
        true ->
            {_, User} = lists:keyfind(rpcuser_pub, 1, CfgList),
            {_, Password} = lists:keyfind(rpcpassword_pub, 1, CfgList),
            {_, HostPort} = lists:keyfind(rpchost_pub, 1, CfgList);
        false ->
            {_, User} = lists:keyfind(rpcuser, 1, CfgList),
            {_, Password} = lists:keyfind(rpcpassword, 1, CfgList),
            {_, HostPort} = lists:keyfind(rpchost, 1, CfgList)
    end,
    % Achain不支持最后的斜杠
    %Url = unicode:characters_to_list(<<"http://", HostPort/binary, "/">>),
    Url = unicode:characters_to_list(<<"http://", HostPort/binary>>),
    Body = jiffy:encode(JsonReq),
    ContentType = "application/json",
    Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(<<User/binary, ":", Password/binary>>)}],
    Request = {Url, Headers, ContentType, Body},
    HttpOptions = [{timeout, ?HTTP_REQUEST_TIMEOUT},
                   {connect_timeout, ?HTTP_CONNECTION_TIMEOUT},
                   {autoredirect, true}],
    Options = [],
    %?DBG("Request: ~p, HttpOptions: ~p Options: ~p with Url: ~ts", [Request, HttpOptions, Options, Url]),
    %?DBG("Url: ~p~n", [Url]),
    %?DBG("Body: ~p~n", [Body]),
    {ok, _Response = {{_HttpVersion, StatusCode, StatusText}, _RespHeaders, RespBody}} =
        httpc:request(post, Request, HttpOptions, Options),
    RespDecoded0 = (catch jiffy:decode(unicode:characters_to_binary(RespBody), [return_maps])),
    RespDecoded = case is_map(RespDecoded0) of
                      true -> RespDecoded0;
                      false -> #{<<"error">> => {jiffy_decode_error, RespBody}}
                  end,
    %?DBG("StatusCode: ~p, Response: ~p", [StatusCode, RespDecoded]),
    case StatusCode of
        GoodStatus when (GoodStatus >= 200) and (GoodStatus =< 299);
                        (GoodStatus =:= 500 andalso Chain =:= <<"act">>) ->
            case maps:is_key(<<"error">>, RespDecoded) of
                true ->
                    case maps:get(<<"error">>, RespDecoded, RespDecoded) of
                        null -> {ok, maps:get(<<"result">>, RespDecoded, RespDecoded)};
                        Error -> {error, Error}
                    end;
                false -> {ok, maps:get(<<"result">>, RespDecoded, RespDecoded)}
            end;
        _ ->
            ?ERR("Http returned status ~p ~ts from Request: ~p with Response: ~p",
                 [StatusCode, StatusText, Request, RespDecoded]),
            {error, maps:get(<<"error">>, RespDecoded, RespDecoded)}
    end.

get_seed() ->
    Seed =
        case get(seed) of
            undefined -> crypto:strong_rand_bytes(?SEED_BYTES);
            Seed_ -> Seed_
        end,
    put(seed, increment_seed(Seed)),
    Seed.

increment_seed(<<Num:?SEED_BYTES/unsigned-integer-unit:8>>) ->
    <<(Num + 1):?SEED_BYTES/unsigned-integer-unit:8>>;
increment_seed(Bin) when is_binary(Bin) ->
    crypto:strong_rand_bytes(?SEED_BYTES).

get_symbols_by_chain(Chain) ->
    {ok, Symbols} = application:get_env(exchange_common, symbols),
    L = [?CHAIN_TOKEN(atom_to_binary(Symbol, utf8)) || Symbol <- Symbols],
    lists:usort([T || {C, T} <- L, C =:= Chain]).

