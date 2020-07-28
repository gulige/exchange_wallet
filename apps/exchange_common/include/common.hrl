%%%------------------------------------------------
%%% File    : common.hrl
%%% Description: 公共定义
%%%------------------------------------------------

% use lager log system: debug, info, notice, warning, error, critical, alert, emergency
-ifdef(debug).
    -define(DEBUG(F, A), lager:debug(F, A)).
    -define(DBG(Str, Args), lager:info(Str, Args)).
    -define(DBGS(Str), lager:info(Str)).
-else.
    -define(DEBUG(F, A), ok).
    -define(DBG(Str, Args), ok).
    -define(DBGS(Str), ok).
-endif.
-define(INFO(F, A), lager:info(F, A)).
-define(ERR(F, A), lager:error(F, A)).

%%ETS
-define(ETSRC, {read_concurrency, true}).
-define(ETSWC, {write_concurrency, true}).

-define(T2B(T), util:term_to_bitstring(T)).

-define(CHAIN(Symbol), begin [H|_] = binary:split(Symbol, [<<"_">>]), H end).
-define(CHAIN_TOKEN(Symbol), begin [C,T] = binary:split(Symbol, [<<"_">>]), {C,T} end).

-define(SVRTYPE_WALLET_NODE, 1).
-define(SVRTYPE_SIGN_NODE, 2).

% 环境
-define(ENV_DEV, dev).
-define(ENV_BETA, beta).
-define(ENV_STAGE, stage).
-define(ENV_PROD, prod).

% 地址类型
-define(ADDR_TYPE_ALL, -1). % 所有地址类型
-define(ADDR_TYPE_NORMAL, 0).
-define(ADDR_TYPE_GATHER, 1).
-define(ADDR_TYPE_FEE, 2).

