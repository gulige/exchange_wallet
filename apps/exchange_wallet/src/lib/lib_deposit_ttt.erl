%%%--------------------------------------
%%% @Module  : lib_deposit_ttt
%%% @Description: TrustNote存币相关处理
%%%--------------------------------------
-module(lib_deposit_ttt).

-export([do_chain/0]).

-include_lib("exchange_common/include/common.hrl").

-include("exchange_wallet.hrl").

-define(CHAIN, <<"ttt">>).


do_chain() ->
    try
        {SinceMCI, _Done} = lib_block_scan_mark:get_scan_mark(?CHAIN),
        TxList = chain_ttt:get_all_deposit_txs_from_outside_to_my_wallet(SinceMCI),
        lists:foldl(fun({_TxId, _FromAddrList, _ToAddr, _Symbol, _Amount, _Gas, Level, MCI} = Tx, AccMCI) ->
                        NewAccMCI = case AccMCI =:= MCI of
                                        true -> AccMCI;
                                        false -> lib_block_scan_mark:set_scan_mark(?CHAIN, MCI, Level, 0), MCI
                                    end,
                        do_tx(Tx),
                        NewAccMCI
                    end, SinceMCI, TxList),
        ok
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("deposit_ttt exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

do_tx({TxId, FromAddrList, ToAddr, Symbol, Amount0, Gas, _Level, _MCI}) ->
    % 不能捕获异常，否则会导致外层逻辑MCI的推进，这些处理异常的交易就得不到再次的处理
    case lib_tx_store:get_tx(?CHAIN, TxId) of
        undefined ->
            Amount = util:round10d(Amount0),
            % 直接加入tx表，因为前面逻辑已保证确认数是够的，当周期扫描各交易的确认数达到预期后，发送通知给finance模块
            lib_tx_store:new_tx(?CHAIN, TxId, [0, 0, FromAddrList, ToAddr, Symbol, Amount, Gas, Gas, []]);
        _ ->
            % 已登记在案，说明已成功处理
            void
    end.

