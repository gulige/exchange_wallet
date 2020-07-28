%%%--------------------------------------
%%% @Module  : chain_asch
%%% @Description: 阿希链的访问接口
%%%--------------------------------------
-module(chain_asch).
-compile(export_all).

-export([start_node/0,
         stop_node/0,
         need_gather/0,
         get_new_address/0,
         get_balance/2,
         make_tx/4,
         sign_tx/2,
         send_tx/1,
         get_latest_block_id/0,
         get_block/1,
         get_tx/1,
         import_priv_key/1]).

-include("common.hrl").

-define(CHAIN, <<"asch">>).
-define(SYMBOL, <<"xas">>).
-define(FACTOR, 100000000).

-define(SYMBOL_ENS, <<"ens">>).
-define(SYMBOL_EMT, <<"emt">>).


-define(HTTP_REQUEST_TIMEOUT, 60000 * 10).

-define(ERRNO_HTTP_REQ_FAILED, -1). % 请求失败
-define(ERRNO_HTTP_REQ_TIMEOUT, -2). % 请求超时
-define(ERRNO_HTTP_REQ_SERVER_LOGIC, -3). % 服务器逻辑返回错误

% 阿希链不支持utf8编码
-define(JSON_CONTENT, {"Content-Type", "application/json"}).


start_node() ->
    ?INFO("starting asch node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/asch; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/asch/stop.sh"),
    ok.

need_gather() ->
    % 采用余额汇总流程
    true.

get_new_address() ->
    % 因为转账时只能用助记词，所以我们记录助记词，而不是私钥（privateKey）
    {ok, #{<<"address">> := Addr, <<"secret">> := PrivKey}} = http_request(<<"accounts/new">>),
    {Addr, PrivKey}.

get_balance(?SYMBOL, Addr) ->
    % 不能指定确认数
    %lib_chain_cfg:confirm_count(?CHAIN),
    {ok, #{<<"balance">> := Balance}} = http_request(<<"accounts/getBalance?address=", Addr/binary>>),
    Balance / ?FACTOR;
get_balance(Symbol, Addr) ->
    {ok, #{<<"balance">> := #{<<"balance">> := B}}} = http_request(<<"uia/balances/", Addr/binary, "/", (get_inner_symbol(Symbol))/binary>>),
    Balance = util:round10d(util:binary_to_float(B) / ?FACTOR),
    Balance.

make_tx(?SYMBOL, ToAddr, [{FromAddr, Amount}], Gas) ->
    case lib_keystore:read_key(?CHAIN, FromAddr) of
        <<>> -> no_priv_key_found;
        PrivKey ->
            case http_request(<<"transactions">>, [{secret, PrivKey}, {amount, trunc(util:round10d(Amount - Gas) * ?FACTOR)}, {recipientId, ToAddr}]) of
                {ok, #{<<"transactionId">> := TxId}} ->
                    % 阿希链在这里已签过名，不需要再调用sign_tx，并且也已经广播
                    TxId
            end
    end;
make_tx(Symbol, ToAddr, AddrToDeltaAmountPairs0, Gas) ->
    [{FromAddr, Amount}] = [{Addr, DeltaAmount} || {Addr, DeltaAmount} <- AddrToDeltaAmountPairs0, DeltaAmount > 0],
    XasBalance = get_balance(?SYMBOL, FromAddr),
    case XasBalance >= 0.1 of
        true ->
            case lib_keystore:read_key(?CHAIN, FromAddr) of
                <<>> -> no_priv_key_found;
                PrivKey ->
                    AmountInt = trunc(util:round10d(Amount) * ?FACTOR),
                    AmountBin = integer_to_binary(AmountInt),
                    case http_request(<<"uia/transfers">>, [{secret, PrivKey}, {amount, AmountBin}, {recipientId, ToAddr}, {currency, get_inner_symbol(Symbol)}]) of
                        {ok, #{<<"transactionId">> := TxId}} ->
                            % 阿希链在这里已签过名，不需要再调用sign_tx，并且也已经广播
                            TxId
                    end
            end;
        false ->
            case get(fee_charging) of
                undefined ->
                    % 从手续费地址转点手续费到该地址
                    case lib_mongo:find_one(?CHAIN, <<"address">>, #{<<"addr_type">> => ?ADDR_TYPE_FEE}, #{<<"_id">> => true}) of
                        #{<<"_id">> := FeeAddr} -> % 手续费地址只有一个
                            FeeTxId = make_tx(?SYMBOL, FromAddr, [{FeeAddr, 0.2}], 0.1),
                            ?INFO("fee charging for chain: asch, tx_id = ~s~n", [FeeTxId]);
                        _ ->
                            throw({error, <<"no fee address found for chain: asch">>})
                    end,
                    put(fee_charging, true),
                    % 等15秒
                    timer:sleep(15000),
                    make_tx(Symbol, ToAddr, [{FromAddr, Amount}], Gas);
                _ ->
                    % 等15秒
                    timer:sleep(15000),
                    make_tx(Symbol, ToAddr, [{FromAddr, Amount}], Gas)
            end
    end.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx) ->
    SignedTx.

get_latest_block_id() ->
    {ok, #{<<"height">> := Height}} = http_request(<<"blocks/getheight">>),
    {ok, #{<<"block">> := #{<<"id">> := Id}}} = http_request(<<"blocks/get?height=", (integer_to_binary(Height))/binary>>),
    Id.

get_block(BlockId) ->
    {ok, #{<<"block">> := #{<<"height">> := Height, <<"confirmations">> := Confirmations0, <<"numberOfTransactions">> := TxNum} = ResMap}} =
        http_request(<<"blocks/get?id=", BlockId/binary>>),
    TxIds = case TxNum > 0 of
                false -> [];
                true ->
                    {ok, #{<<"transactions">> := TxList}} = http_request(<<"transactions?blockId=", BlockId/binary>>),
                    [TxId || #{<<"id">> := TxId} <- TxList]
            end,
    NextBlockId =
        case http_request(<<"blocks/get?height=", (integer_to_binary(Height + 1))/binary>>) of
            {ok, #{<<"block">> := #{<<"id">> := NextBlockId_}}} -> NextBlockId_;
            _ -> undefined
        end,
    PrevBlockId = case maps:is_key(<<"previousBlock">>, ResMap) of
                      true -> #{<<"previousBlock">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    Confirmations = case is_binary(Confirmations0) of
                        true -> binary_to_integer(Confirmations0);
                        false -> Confirmations0
                    end,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

get_tx(TxId) ->
    case http_request(<<"transactions/get?id=", TxId/binary>>) of
        {ok, #{<<"transaction">> := #{<<"type">> := TxType} = ResMap}} ->
            case TxType of
                _ when TxType =:= 0; TxType =:= 14 ->
                    #{<<"senderId">> := FromAddr, <<"recipientId">> := ToAddr,
                      <<"amount">> := Amount0, <<"fee">> := Gas,
                      <<"confirmations">> := Confirmations0,
                      <<"asset">> := Asset} = ResMap,
                    Confirmations = case is_binary(Confirmations0) of
                                        true -> binary_to_integer(Confirmations0);
                                        false -> Confirmations0
                                    end,
                    {Symbol, Amount1} =
                        case TxType of
                            0 -> {?SYMBOL, Amount0};
                            14 ->
                                #{<<"uiaTransfer">> := #{<<"currency">> := C, <<"amount">> := A}} = Asset,
                                {get_symbol(C), A}
                        end,
                    Amount = case is_binary(Amount1) of
                                 true -> binary_to_integer(Amount1);
                                 false -> Amount1
                             end,
                    {[FromAddr], [ToAddr], Symbol, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
                _ ->
                    tx_type_we_dont_care
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> undefined;
                false -> tx_type_we_dont_care
            end
    end.

import_priv_key(_PrivKey) ->
    unsupported.

get_inner_symbol(?SYMBOL_ENS) -> <<"ENDLESS.ENS">>;
get_inner_symbol(?SYMBOL_EMT) -> <<"EBOOOM.EMT">>;
get_inner_symbol(_) -> <<>>.

get_symbol(<<"ENDLESS.ENS">>) -> ?SYMBOL_ENS;
get_symbol(<<"EBOOOM.EMT">>) -> ?SYMBOL_EMT;
get_symbol(_) -> <<>>.

get_http_url(IsPub) ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, jsonrpc_asch),
    case IsPub of
        true ->
            {_, Url} = lists:keyfind(rpchost_pub, 1, CfgList);
        false ->
            {_, Url} = lists:keyfind(rpchost, 1, CfgList)
    end,
    <<"http://", Url/binary, "/api/">>.

http_request(ParamsUrl) ->
    http_request(ParamsUrl, [], false).

http_request(ParamsUrl, Params) ->
    http_request(ParamsUrl, Params, false).

http_request(ParamsUrl, Params, IsPub) when is_list(Params) ->
    UrlPrefix = get_http_url(IsPub),
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
                    case maps:get(<<"success">>, JsonObject) of
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

