%%%--------------------------------------
%%% @Module  : lib_sign
%%% @Description: 签名相关处理
%%%--------------------------------------
-module(lib_sign).

-export([make_tx/5, import_keys/1]).

-include_lib("exchange_common/include/common.hrl").

make_tx(Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas) ->
    ?DBG("making a signed tx:Chain=~p, Symbol=~p, ToAddr=~p, AddrToDeltaAmountPairs=~p, Gas=~p...~n",
         [Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas]),
    try
        lib_chain:make_tx(Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas)
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("making a signed tx exception:Chain=~p, Symbol=~p, ToAddr=~p, AddrToDeltaAmountPairs=~p, Gas=~p~nerr_msg=~p~nstack=~p~n",
                 [Chain, Symbol, ToAddr, AddrToDeltaAmountPairs, Gas, ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

% 临时将sign节点上的rpchost修改为wallet节点上的rpchost指向，执行该函数则可以把私钥导入过去（注意链节点的rpcallowip）
import_keys(Chain) ->
    put(import_cnt, 0),
    F = fun(#{<<"address">> := Addr}) ->
            PrivKey = lib_keystore:read_key(Chain, Addr),
            lib_chain:import_priv_key(Chain, PrivKey),
            NowCnt = get(import_cnt) + 1,
            put(import_cnt, NowCnt),
            case NowCnt rem 1000 of
                0 -> ?INFO("~p imported~n", [NowCnt]);
                _ -> void
            end
        end,
    lib_mongo_priv:iterate(Chain, <<"keystore_", Chain/binary>>, #{}, #{<<"_id">> => false, <<"address">> => true}, F),
    ok.

