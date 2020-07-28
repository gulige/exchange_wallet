%%%--------------------------------------
%%% @Module  : lib_finance_request
%%% @Description: finance模块请求处理
%%%--------------------------------------
-module(lib_finance_request).

-export([get_app_id/0, get_url/1, do/3]).

-include_lib("exchange_common/include/common.hrl").

-include("exchange_wallet.hrl").

%% 和Finance模块交互的http接口（开发环境）
-define(FINANCE_URL_PREFIX_DEV, "http://inner.game.org:8157/finance/").
-define(FINANCE_APP_ID_DEV, <<"game">>).
-define(FINANCE_APP_SECRET_DEV, <<"xxx">>).

%% 和Finance模块交互的http接口（test环境）
-define(FINANCE_URL_PREFIX_TEST, "http://47.88.170.203:8157/finance/").
-define(FINANCE_APP_ID_TEST, <<"game">>).
-define(FINANCE_APP_SECRET_TEST, <<"xxx">>).

%% 和Finance模块交互的http接口（产品环境）
-define(FINANCE_URL_PREFIX_PROD, "http://172.21.131.210:8151/finance/").
-define(FINANCE_APP_ID_PROD, <<"game-online">>).
-define(FINANCE_APP_SECRET_PROD, <<"xxx">>).

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).

get_url(Action) when is_list(Action) ->
    Prefix = case plat:env() of
        ?ENV_PROD -> ?FINANCE_URL_PREFIX_PROD;
        _ -> ?FINANCE_URL_PREFIX_DEV
    end,
    Prefix ++ Action ++ "/".

get_app_id() ->
    case plat:env() of
        ?ENV_PROD -> ?FINANCE_APP_ID_PROD;
        _ -> ?FINANCE_APP_ID_DEV
    end.

get_app_secret() ->
    case plat:env() of
        ?ENV_PROD -> ?FINANCE_APP_SECRET_PROD;
        _ -> ?FINANCE_APP_SECRET_DEV
    end.

do(Url, BinToSign, Params0) ->
    Sign0 = crypto:hmac(sha, get_app_secret(), BinToSign),
    Sign = list_to_binary(lists:flatten([io_lib:format("~2.16.0b",[Byte]) || Byte <- binary_to_list(Sign0)])),
    Params = lists:keysort(1, [{sign, Sign} | Params0]),
    JsonParams = jiffy:encode({Params}),
    ?DBG("JsonParams: ~p~n", [JsonParams]),
    case ibrowse:send_req(Url, [?JSON_CONTENT], post, JsonParams) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    {JsonObject} = jiffy:decode(Body),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case lists:keyfind(<<"errno">>, 1, JsonObject) of
                        {_, <<"000">>} -> % 成功
                            case lists:keyfind(<<"data">>, 1, JsonObject) of
                                {_, Data} -> {ok, Data};
                                _ -> {ok, no_data}
                            end;
                        {_, ErrCode} -> % 失败
                            {_, ErrMsg} = lists:keyfind(<<"errmsg">>, 1, JsonObject),
                            {error, ErrCode, ErrMsg}
                    end;
                _ ->
                    throw({error, ?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

