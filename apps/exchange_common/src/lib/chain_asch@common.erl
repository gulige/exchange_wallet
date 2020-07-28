%%%--------------------------------------
%%% @Module  : chain_asch@common
%%% @Description: 阿希链侧链访问的公共模块
%%%--------------------------------------
-module(chain_asch@common).
-compile(export_all).

-include("common.hrl").

-define(FACTOR, 100000000).

-define(HTTP_REQUEST_TIMEOUT, 60000 * 10).

-define(ERRNO_HTTP_REQ_FAILED, -1). % 请求失败
-define(ERRNO_HTTP_REQ_TIMEOUT, -2). % 请求超时
-define(ERRNO_HTTP_REQ_SERVER_LOGIC, -3). % 服务器逻辑返回错误

% 阿希链不支持utf8编码
-define(JSON_CONTENT, {"Content-Type", "application/json"}).


get_new_address(SideChain, _DAppId) ->
    % 因为转账时只能用助记词，所以我们记录助记词，而不是私钥（privateKey）
    {ok, #{<<"address">> := Addr, <<"secret">> := PrivKey}} = http_request(SideChain, <<>>, <<"accounts/new">>),
    {Addr, PrivKey}.

get_balance(SideChain, DAppId, Symbol, Addr) ->
    % 不能指定确认数
    {ok, #{<<"account">> := #{<<"balances">> := BalanceList}, <<"success">> := true}} =
        http_request(SideChain, DAppId, <<"accounts/", Addr/binary>>),
    case BalanceList of
        [] -> 0;
        _ ->
            case [binary_to_integer(B) || #{<<"currency">> := Currency, <<"balance">> := B} <- BalanceList, Currency =:= Symbol] of
                [] -> 0;
                [Balance] -> Balance / ?FACTOR
            end
    end.

make_tx(SideChain, DAppId, Symbol, ToAddr, [{FromAddr, Amount}], Gas) ->
    case lib_keystore:read_key(SideChain, FromAddr) of
        <<>> -> no_priv_key_found;
        PrivKey0 -> % fee目前固定为10000000
            PrivKey = http_uri:encode(PrivKey0),
            AmountBin = integer_to_binary(trunc(util:round10d(Amount - Gas) * ?FACTOR)),
            {ok, SignedJsonTx} = http_sign_request(SideChain, DAppId, <<"symbol=", Symbol/binary, "&amount=", AmountBin/binary, "&toaddr=", ToAddr/binary, "&secret=", PrivKey/binary>>),
            % 阿希链在这里已签过名，不需要再调用sign_tx
            jiffy:encode(SignedJsonTx)
    end.

send_tx(SideChain, DAppId, SignedTx) ->
    SignedJsonTx = jiffy:decode(SignedTx),
    case http_request(SideChain, DAppId, <<"transactions/signed">>, [{transaction, SignedJsonTx}]) of
        {ok, #{<<"transactionId">> := TxId}} ->
            TxId
    end.

get_latest_block_id(SideChain, DAppId) ->
    {ok, #{<<"height">> := Height}} = http_request(SideChain, DAppId, <<"blocks/height">>),
    {ok, #{<<"block">> := #{<<"id">> := Id}}} = http_request(SideChain, DAppId, <<"blocks/", (integer_to_binary(Height))/binary>>),
    Id.

get_block(SideChain, DAppId, BlockId) ->
    {ok, #{<<"block">> := #{<<"height">> := Height} = ResMap}} =
        http_request(SideChain, DAppId, <<"blockById/", BlockId/binary>>),
    {ok, #{<<"trans">> := TxList}} = http_request(SideChain, DAppId, <<"trans/", (integer_to_binary(Height))/binary>>),
    TxIds = [TxId || #{<<"id">> := TxId, <<"type">> := Type} <- TxList, Type =:= 3], % 先过滤3类型，免得后面白查
    NextBlockId =
        case http_request(SideChain, DAppId, <<"blocks/", (integer_to_binary(Height + 1))/binary>>) of
            {ok, #{<<"block">> := #{<<"id">> := NextBlockId_}}} -> NextBlockId_;
            _ -> undefined
        end,
    PrevBlockId = case maps:is_key(<<"prevBlockId">>, ResMap) of
                      true -> #{<<"prevBlockId">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    {ok, #{<<"height">> := TopHeight}} = http_request(SideChain, DAppId, <<"blocks/height">>),
    Confirmations = TopHeight - Height + 1,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

get_tx(SideChain, DAppId, TxId) ->
    case http_request(SideChain, DAppId, <<"transactions/", TxId/binary>>) of
        {ok, #{<<"transaction">> := #{<<"type">> := TxType} = ResMap}} ->
            case TxType of
                3 ->
                    #{<<"senderId">> := FromAddr, <<"args">> := Args, <<"fee">> := Gas0, <<"height">> := Height} = ResMap,
                    [Symbol0, Amount0, ToAddr] = jiffy:decode(Args),
                    M = binary_to_atom(<<"chain_", SideChain/binary>>, utf8),
                    Symbol = M:get_symbol(Symbol0),
                    Symbols = lib_chain:get_symbols_by_chain(SideChain),
                    case lists:member(Symbol, Symbols) of
                        true ->
                            {ok, #{<<"height">> := TopHeight}} = http_request(SideChain, DAppId, <<"blocks/height">>),
                            Confirmations = TopHeight - Height + 1,
                            Amount = case is_binary(Amount0) of
                                         true -> binary_to_integer(Amount0);
                                         false -> Amount0
                                     end,
                            Gas = case is_binary(Gas0) of
                                      true -> binary_to_integer(Gas0);
                                      false -> Gas0
                                  end,
                            {[FromAddr], [ToAddr], Symbol, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
                        false ->
                            symbol_we_dont_care
                    end;
                _ ->
                    tx_type_we_dont_care
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> undefined;
                false -> tx_type_we_dont_care
            end
    end.

get_http_url(SideChain, DAppId, IsPub) ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, binary_to_atom(<<"jsonrpc_", SideChain/binary>>, utf8)),
    case IsPub of
        true ->
            {_, Url} = lists:keyfind(rpchost_pub, 1, CfgList);
        false ->
            {_, Url} = lists:keyfind(rpchost, 1, CfgList)
    end,
    case DAppId of
        <<>> -> <<"http://", Url/binary, "/api/">>;
        _ -> <<"http://", Url/binary, "/api/dapps/", DAppId/binary, "/">>
    end.

http_request(SideChain, DAppId, ParamsUrl) ->
    http_request(SideChain, DAppId, ParamsUrl, [], false).

http_request(SideChain, DAppId, ParamsUrl, Params) ->
    http_request(SideChain, DAppId, ParamsUrl, Params, false).

http_request(SideChain, DAppId, ParamsUrl, Params, IsPub) when is_list(Params) ->
    UrlPrefix = get_http_url(SideChain, DAppId, IsPub),
    Url = <<UrlPrefix/binary, ParamsUrl/binary>>,
    Method = case Params of
                 [] -> get;
                 _ -> put
             end,
    JsonParams = jiffy:encode({Params}),
    ?DBG("Url: ~p, Params: ~p~n", [Url, JsonParams]),
    case ibrowse:send_req(binary_to_list(Url), [?JSON_CONTENT], Method, JsonParams, [], ?HTTP_REQUEST_TIMEOUT) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case maps:get(<<"success">>, JsonObject, false) of
                        true -> % 成功
                            {ok, JsonObject};
                        false -> % 失败
                            {error, ?ERRNO_HTTP_REQ_SERVER_LOGIC, maps:get(<<"error">>, JsonObject)}
                    end;
                _ ->
                    throw({error, ?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

get_http_sign_url(SideChain, _DAppId) ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, binary_to_atom(<<"jsonrpc_", SideChain/binary>>, utf8)),
    {_, Url} = lists:keyfind(http_sign_host, 1, CfgList),
    <<"http://", Url/binary, "/sign?">>.

http_sign_request(SideChain, DAppId, ParamsUrl) ->
    Url0 = get_http_sign_url(SideChain, DAppId),
    Url = <<Url0/binary, ParamsUrl/binary>>,
    ?DBG("Url: ~p~n", [Url]),
    case ibrowse:send_req(binary_to_list(Url), [], get) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("Signed JsonObject: ~p~n", [JsonObject]),
                    case catch maps:get(<<"signature">>, JsonObject) of
                        {'EXIT',{{badkey,<<"signature">>}, _}} -> % 失败
                            {error, JsonObject};
                        _ -> % 成功
                            {ok, JsonObject}
                    end;
                _ ->
                    throw({error, ?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

