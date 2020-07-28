%%%--------------------------------------
%%% @Module  : lib_address
%%% @Description: 地址相关处理
%%%--------------------------------------
-module(lib_address).

-export([gen/2, gen_gather_addr/1, gen_fee_addr/1]).

-include_lib("exchange_common/include/common.hrl").

%% 生成一批地址
gen(Chain, Num) ->
    {MicroSeconds, Res} = timer:tc(fun() -> do_gen(Chain, Num, 0, Num, 0) end),
    {MicroSeconds div 1000000, Res}.

%% 生成一个汇总用地址
gen_gather_addr(Chain) ->
    ?DBG("generating 1 ~s address for gathering use...~n", [Chain]),
    try
        lib_chain:start_node(Chain),
        % 我们生成地址用的钱包不加密，所以不需要walletpassphrase解锁
        gen_one(Chain, ?ADDR_TYPE_GATHER)
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("generate 1 ~s address (for gathering use) exception:~nerr_msg=~p~nstack=~p~n",
                 [Chain, ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

%% 生成一个手续费地址
gen_fee_addr(Chain) ->
    ?DBG("generating 1 ~s address for fee use...~n", [Chain]),
    try
        lib_chain:start_node(Chain),
        % 我们生成地址用的钱包不加密，所以不需要walletpassphrase解锁
        gen_one(Chain, ?ADDR_TYPE_FEE)
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("generate 1 ~s address (for fee use) exception:~nerr_msg=~p~nstack=~p~n",
                 [Chain, ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

do_gen(Chain, Num, DoneCount, TargetCount, RetryCount) ->
    ?DBG("generating ~s:~p addresses...~n", [Chain, Num]),
    try
        lib_chain:start_node(Chain),
        % 我们生成地址用的钱包不加密，所以不需要walletpassphrase解锁
        % 分几批并发生成
        NThreads = 50,
        [H | T] = lists:duplicate(NThreads, Num div NThreads),
        L0 = [H + (Num - H * NThreads) | T],
        L = [I || I <- L0, I > 0],
        F = fun(N) -> lists:sum([gen_one(Chain, ?ADDR_TYPE_NORMAL) || _I <- lists:seq(1, N)]) end,
        NowDoneCount = lists:sum(util:pmap(F, L)) + DoneCount,
        case NowDoneCount >= TargetCount of
            true ->
                NowDoneCount;
            false ->
                case RetryCount < 10 of
                    true ->
                        do_gen(Chain, TargetCount - NowDoneCount, NowDoneCount, TargetCount, RetryCount + 1);
                    false ->
                        NowDoneCount
                end
        end
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("generate ~s:~p addresses exception:~nerr_msg=~p~nstack=~p~n",
                 [Chain, Num, ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

gen_one(Chain, AddrType) ->
    try
        % 获取地址和对应私钥，并写入keystore
        {Addr, PrivKey} = lib_chain:get_new_address(Chain),
        lib_mongo:write_one(Chain, <<"address">>, #{<<"_id">> => Addr, <<"addr_type">> => AddrType}),
        lib_keystore:write_key(Chain, Addr, PrivKey),
        1
    catch
        _:Err ->
            ?ERR("lib_address:get_one failed: ~p~n", [Err]),
            0
    end.

