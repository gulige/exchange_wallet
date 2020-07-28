%%%--------------------------------------
%%% @Module  : lib_sign_node
%%% @Description: rpc签名节点去处理的逻辑
%%%--------------------------------------
-module(lib_sign_node).

-export([gen_addrs/2,
         make_tx/5]).

-include_lib("exchange_common/include/common.hrl").

gen_addrs(Chain, Num) ->
    ?DBG("rpcing sign node to generate ~p:~p addresses...~n", [Chain, Num]),
    try
        lib_rpc:rpc_sign_node(Chain, lib_address, gen, [Chain, Num]),
        % 地址生成完毕，加载所有地址进内存
        lib_chain_addrs:load_addrs(Chain),
        ok
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("rpcing sign node to generate ~p:~p addresses exception:~nerr_msg=~p~nstack=~p~n",
                 [Chain, Num, ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

make_tx(Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas) ->
    ?DBG("rpcing sign node to make a signed tx...~n", []),
    try
        lib_rpc:rpc_sign_node(Chain, lib_sign, make_tx, [Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas])
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("rpcing sign node to make a signed tx exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

