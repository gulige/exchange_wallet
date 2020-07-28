%%%-----------------------------------
%%% @Module  : plat
%%% @Description: 环境等平台判断接口
%%%-----------------------------------

-module(plat).
-export([env/0]).

-include("common.hrl").

%% 获取环境
%% 据此区分不同的逻辑分支，如下：
%% case plat:env() of
%%     ?ENV_DEV -> ...
%%     ?ENV_BETA -> ...
%%     ?ENV_STAGE -> ...
%%     ?ENV_PROD -> ...
%%     _ -> ...
%% end
env() ->
    case application:get_env(exchange_common, env) of
        {ok, Env} -> Env;
        undefined -> ?ENV_DEV
    end.

