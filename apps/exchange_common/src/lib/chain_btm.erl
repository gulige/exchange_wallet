%%%--------------------------------------
%%% @Module  : chain_btm
%%% @Description: 比原链的访问接口
%%%--------------------------------------
-module(chain_btm).
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

-define(CHAIN, <<"btm">>).
-define(SYMBOL, <<"btm">>).
-define(FACTOR, 100000000). % 10的8次方

-define(ERRNO_HTTP_REQ_FAILED, -1). % 请求失败
-define(ERRNO_HTTP_REQ_TIMEOUT, -2). % 请求超时
-define(ERRNO_HTTP_REQ_SERVER_LOGIC, -3). % 服务器逻辑返回错误

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).

-define(ACCESS_TOKEN_ID, <<"access_token_game">>).
-define(KEY_NAME, <<"key_game">>).
-define(KEY_PASSWORD, <<"xxx">>).
-define(ACCOUNT_NAME, <<"game">>).

-define(ASSETID_BTM, <<"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff">>).

-define(TX_EXPIRED_SECONDS, 300).


start_node() ->
    ?INFO("starting btm node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/btm; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/btm/stop.sh"),
    ok.

need_gather() ->
    % UTXO模型，类似比特币，可以多个地址一起转账，因此不需要余额汇总
    false.

create_account() ->
    {ok, #{<<"xpub">> := XPub}} =
        http_request("create-key", [{alias, ?KEY_NAME}, {password, ?KEY_PASSWORD}]),
    {ok, #{<<"id">> := AccountId, <<"alias">> := AccountName}} =
        http_request("create-account", [{root_xpubs, [XPub]}, {alias, ?ACCOUNT_NAME}, {quorum, 1}]),
    {AccountId, AccountName}.

get_new_address() ->
    {ok, #{<<"address">> := Addr, <<"control_program">> := PrivKey}} =
        http_request("create-account-receiver", [{account_alias, ?ACCOUNT_NAME}]),
    {Addr, PrivKey}.

% 注意，地址为空，得到的是整个钱包内该币种的余额
get_balance(Symbol, <<>>) ->
    {ok, L} = http_request("list-balances", []),
    case [Amount || #{<<"account_alias">> := AccountName, <<"asset_id">> := AssetId, <<"amount">> := Amount} <- L,
                    AccountName =:= ?ACCOUNT_NAME, AssetId =:= asset_id(Symbol)] of
        [] -> 0;
        [Amt] -> Amt / ?FACTOR
    end;
get_balance(Symbol, Addr) ->
    {ok, L} = http_request("list-unspent-outputs", []),
    case [Amount || #{<<"account_alias">> := AccountName, <<"asset_id">> := AssetId, <<"address">> := Address, <<"amount">> := Amount} <- L,
                    AccountName =:= ?ACCOUNT_NAME, AssetId =:= asset_id(Symbol), Address =:= Addr] of
        [] -> 0;
        AmtL -> lists:sum(AmtL) / ?FACTOR
    end.

make_tx(Symbol, ToAddr, [{_FromAddr, Amount}], Gas) ->
    {ok, RawTx} =
        http_request("build-transaction", [{base_transaction, null}, {ttl, 0}, {time_range, util:unixtime() + ?TX_EXPIRED_SECONDS},
                                           {actions, [{[{account_alias, ?ACCOUNT_NAME}, {amount, trunc(Amount * ?FACTOR)},
                                                        {asset_id, asset_id(Symbol)}, {type, spend_account}]},
                                                      {[{amount, trunc(util:round10d(Amount - Gas) * ?FACTOR)}, {asset_id, asset_id(Symbol)},
                                                      {address, ToAddr}, {type, control_address}]}]}]),
    {ok, #{<<"sign_complete">> := true, <<"transaction">> := #{<<"raw_transaction">> := SignedTx}}} =
        http_request("sign-transaction", [{password, ?KEY_PASSWORD}, {transaction, RawTx}]),
    %{ok, GasObj} = http_request("estimate-transaction-gas", [{transaction_template, RawTx}]),
    {ok, #{<<"tx_id">> := TxId}} = http_request("submit-transaction", [{raw_transaction, SignedTx}]),
    TxId.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx) ->
    SignedTx.

get_latest_block_id() ->
    {ok, #{<<"block_hash">> := Hash}} = http_request("get-block-hash", []),
    Hash.

get_block(BlockId) ->
    {ok, #{<<"height">> := Height, <<"transactions">> := TxList} = ResMap} =
        http_request("get-block", [{block_hash, BlockId}]),
    PrevBlockId = case maps:is_key(<<"previous_block_hash">>, ResMap) of
                      true -> #{<<"previous_block_hash">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    TxIds = [TxId || #{<<"id">> := TxId} <- TxList],
    % 根据块号连续的前提，来确定确认数和后继区块
    {ok, #{<<"block_count">> := TopHeight}} = http_request("get-block-count", []),
    Confirmations = TopHeight - Height + 1,
    NextBlockId =
        case http_request("get-block", [{block_height, Height + 1}]) of
            {ok, #{<<"height">> := NextHeight, <<"hash">> := NextBlockId_}} ->
                NextHeight = Height + 1,
                NextBlockId_;
            _ -> undefined
        end,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

get_tx(TxId) ->
    case http_request("get-transaction", [{tx_id, TxId}]) of
        {ok, #{<<"inputs">> := Vin, <<"outputs">> := Vout, <<"block_height">> := Height}} ->
            case get_tx_symbol(Vout) of
                unsupported_symbol ->
                    symbol_we_dont_care;
                Symbol ->
                    {ok, #{<<"block_count">> := TopHeight}} = http_request("get-block-count", []),
                    Confirmations = TopHeight - Height + 1,
                    InAddrValPairs0 =
                        [{InAddr, util:round10d(InVal / ?FACTOR)} || #{<<"address">> := InAddr, <<"amount">> := InVal, <<"type">> := <<"spend">>} <- Vin],
                    InAddrValPairs =
                        lists:foldl(fun({InAddr, InVal}, Acc) ->
                                        case lists:keyfind(InAddr, 1, Acc) of
                                            false -> [{InAddr, InVal} | Acc];
                                            {_, InValAcc} -> lists:keyreplace(InAddr, 1, Acc, {InAddr, InValAcc + InVal})
                                        end;
                                       (_, Acc) -> Acc
                                    end, [], InAddrValPairs0),
                    {InAddrList, InValList} = lists:unzip(InAddrValPairs),
                    InTotalVal = lists:sum(InValList),
                    OutAddrValPairs0 =
                        [{Addr, util:round10d(Val / ?FACTOR)} || #{<<"address">> := Addr, <<"amount">> := Val, <<"type">> := <<"control">>} <- Vout],
                    {OutAddrValPairs, Change} =
                        lists:foldl(fun({OutAddr, OutVal}, {OutAddrValPairsAcc, ChangeAcc}) ->
                                        case lists:member(OutAddr, InAddrList) of
                                            true -> {OutAddrValPairsAcc, ChangeAcc + OutVal};
                                            false ->
                                                case lists:keyfind(OutAddr, 1, OutAddrValPairsAcc) of
                                                    false -> {[{OutAddr, OutVal} | OutAddrValPairsAcc], ChangeAcc};
                                                    {_, OutValAcc} -> {lists:keyreplace(OutAddr, 1, OutAddrValPairsAcc, {OutAddr, OutValAcc + OutVal}), ChangeAcc}
                                                end
                                        end
                                    end, {[], 0}, OutAddrValPairs0),
                    {OutAddrList, OutValList} = lists:unzip(OutAddrValPairs),
                    OutTotalVal = lists:sum(OutValList),
                    Gas = util:round10d(InTotalVal - OutTotalVal - Change),
                    {InAddrList, OutAddrList, Symbol, OutValList, Gas, Confirmations}
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> temporarily_not_found;
                false -> tx_type_we_dont_care
            end
    end.

get_tx_symbol(Vout) ->
    Assets = [Asset || #{<<"asset_id">> := Asset} <- Vout],
    case lists:all(fun(E) -> E =:= ?ASSETID_BTM end, Assets) of
        true -> ?SYMBOL;
        false -> unsupported_symbol
    end.

import_priv_key(_PrivKey) ->
    unsupported.

get_http_url() ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, jsonrpc_btm),
    {_, Url} = lists:keyfind(rpchost, 1, CfgList),
    unicode:characters_to_list(<<"http://", Url/binary, "/">>).

http_request(Method, Params) ->
    Url0 = get_http_url(),
    Url = Url0 ++ Method,
    JsonParams = jiffy:encode({Params}),
    ?DBG("Url: ~p, JsonParams: ~p~n", [Url, JsonParams]),
    AccessTokenPasswd = case plat:env() of
        ?ENV_PROD -> <<>>;
        _ -> <<"4ee577ef516b214716f58d6b4852c7ad903ecf2a5346662876b4c2e61db561ba">>
    end,
    Headers = [?JSON_CONTENT, {"Authorization", "Basic " ++ base64:encode_to_string(<<(?ACCESS_TOKEN_ID)/binary, ":", AccessTokenPasswd/binary>>)}],
    case ibrowse:send_req(Url, Headers, post, JsonParams) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    ?DBG("Body: ~p~n", [Body]),
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case maps:get(<<"status">>, JsonObject) of
                        <<"success">> -> % 成功
                            Data = maps:get(<<"data">>, JsonObject),
                            {ok, Data};
                        <<"fail">> -> % 失败
                            ErrCode = maps:get(<<"code">>, JsonObject),
                            ErrMsg = maps:get(<<"msg">>, JsonObject),
                            {error, ?ERRNO_HTTP_REQ_SERVER_LOGIC, <<ErrCode/binary, ": ", ErrMsg/binary>>}
                    end;
                _ -> % 失败
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    <<"fail">> = maps:get(<<"status">>, JsonObject), % 断言
                    ErrCode = maps:get(<<"code">>, JsonObject),
                    ErrMsg = maps:get(<<"msg">>, JsonObject),
                    {error, ?ERRNO_HTTP_REQ_SERVER_LOGIC, <<ErrCode/binary, ": ", ErrMsg/binary>>}
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

asset_id(?SYMBOL) -> ?ASSETID_BTM;
asset_id(_Symbol) -> <<>>.

