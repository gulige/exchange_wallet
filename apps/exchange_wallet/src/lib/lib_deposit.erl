%%%--------------------------------------
%%% @Module  : lib_deposit
%%% @Description: 存币相关处理
%%%--------------------------------------
-module(lib_deposit).

-export([do_chain/1]).

-include_lib("exchange_common/include/common.hrl").

-include("exchange_wallet.hrl").


do_chain(<<"ttt">>) ->
    lib_deposit_ttt:do_chain();
do_chain(Chain) ->
    %?DBG("processing deposit for chain ~s...~n", [Chain]),
    try
        {BlockId, Done} = lib_block_scan_mark:get_scan_mark(Chain),
        {Height, TxIds, _PrevBlockId, NextBlockId, Confirmations} = lib_chain:get_block(Chain, BlockId),
        NeedConfirmCount = lib_chain_cfg:confirm_count(Chain) + 1, % 多1个确保get_balance能查到
        case Done > 0 of
            true ->
                % 已处理完，处理下一块：但如果还没有下一块，或者有但确认数不够，则不写入block_scan_mark
                case NextBlockId of
                    undefined ->
                        % 还没有下一块，等待下一次处理
                        void;
                    _ ->
                        {N_Height, N_TxIds, N_PrevBlockId, _N_NextBlockId, N_Confirmations} = lib_chain:get_block(Chain, NextBlockId),
                        N_PrevBlockId = BlockId, % 相当于断言
                        case N_Confirmations < NeedConfirmCount of
                            true ->
                                % 确认数不够，等待下一次处理
                                void;
                            false ->
                                % 确认数够了，写入block_scan_mark
                                lib_block_scan_mark:set_scan_mark(Chain, NextBlockId, N_Height, 0),
                                case do_tx_list(Chain, N_TxIds) of
                                    true ->
                                        % 处理完，写入block_scan_mark
                                        lib_block_scan_mark:set_scan_mark(Chain, NextBlockId, N_Height, 1),
                                        % 递归处理
                                        do_chain(Chain);
                                    false ->
                                        % 未处理完，可能出错了，等待下一次再处理
                                        void
                                end
                        end
                end;
            false ->
                % 未处理完，再次处理本块
                case Confirmations < NeedConfirmCount of
                    true ->
                        % 确认数未达到，什么也不做（一般不会走到这里，除非调整了所需确认数）
                        % 另外，这里可能（万一）分叉陷入死循环，记一条errorlog跟踪，此时，
                        % 需要手动将block_scan_mark设到上一个块的完成状态：lib_block_scan_mark:set_scan_mark(Chain, PrevBlockId, 0, 1)，以尝试跳出循环
                        ?ERR("~s pending_block_id=~p, confirmations=~p, need_confirm_count=~p~n", [Chain, BlockId, Confirmations, NeedConfirmCount]),
                        void;
                    false ->
                        % 确认数够了，上一次已经写入block_scan_mark，完成前不用再写入
                        case do_tx_list(Chain, TxIds) of
                            true ->
                                % 处理完，写入block_scan_mark
                                lib_block_scan_mark:set_scan_mark(Chain, BlockId, Height, 1),
                                % 递归处理
                                do_chain(Chain);
                            false ->
                                % 未处理完，可能出错了，等待下一次再处理
                                void
                        end
                end
        end,
        ok
    catch
        throw:ThrownErr ->
            ThrownErr;
        _:ExceptionErr ->
            ?ERR("deposit exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            {exception, ?T2B(ExceptionErr)}
    end.

do_tx_list(Chain, TxIds) ->
    F = fun(TxId) ->
            case lib_tx_store:get_tx(Chain, TxId) of
                undefined ->
                    do_tx(Chain, TxId);
                #{<<"is_withdraw">> := 1, <<"is_internal">> := 1} ->
                    % 内部汇总
                    do_tx(Chain, TxId);
                _ ->
                    % 已登记在案，说明已成功处理
                    true
            end
        end,
    % 全部返回成功，才是成功
    not lists:member(false, [F(TxId) || TxId <- TxIds]).

do_tx(Chain, TxId0) ->
    try
        case lib_chain:get_tx(Chain, TxId0) of
            {FromAddrList, ToAddrList, Symbol, AmountList, Gas, _Confirmations} ->
                case [Addr || Addr <- ToAddrList, lib_chain_addrs:is_addr_ours(Chain, Addr)] of
                    [] -> void;
                    ToOurAddrList -> % 一般只一个地址，但也可能多个地址
                        F = fun(ToAddr) ->
                                TxId = case ToOurAddrList of
                                           [_] -> TxId0;
                                           _ -> <<TxId0/binary, "__", ToAddr/binary>>
                                       end,
                                case lib_tx_store:get_tx(Chain, TxId) of
                                    undefined ->
                                        ToAddrAmountPairs = lists:zip(ToAddrList, AmountList),
                                        {_, Amount0} = lists:keyfind(ToAddr, 1, ToAddrAmountPairs),
                                        Amount = util:round10d(Amount0),
                                        % 检查to_addr是我们的地址
                                        case lib_chain_addrs:is_addr_ours(Chain, ToAddr, ?ADDR_TYPE_NORMAL) of
                                            true ->
                                                % 如果需要，则汇总
                                                case lib_chain:need_gather(Chain) andalso not is_fee_charge(Chain, FromAddrList) of
                                                    true ->
                                                        case lib_chain_addrs:get_addrs_by_type(Chain, ?ADDR_TYPE_GATHER) of
                                                            [GatherAddr|_] -> % 汇总地址只有一个
                                                                % 汇总，生成交易内容
                                                                ToBalance0 = lib_chain:get_balance(Chain, Symbol, ToAddr),
                                                                case ToBalance0 =< 0 of
                                                                    true -> void;
                                                                    false ->
                                                                        {GatherGas0, GasSymbol} = lib_chain_cfg:gas(Symbol, ToBalance0),
                                                                        {ToBalance, GatherGas} =
                                                                            case Symbol =:= GasSymbol of
                                                                                true ->
                                                                                    ToBalance_ = util:round10d(ToBalance0 - GatherGas0),
                                                                                    case ToBalance_ =< 0 of
                                                                                        true ->
                                                                                            throw({error, <<"gather gas not enough, chain=", Chain/binary, ", symbol=", Symbol/binary, ", addr=", ToAddr/binary>>});
                                                                                        false -> void
                                                                                    end,
                                                                                    {GatherGas_, _} = lib_chain_cfg:gas(Symbol, ToBalance_),
                                                                                    {ToBalance_, GatherGas_};
                                                                                false ->
                                                                                    ToGasBalance = lib_chain:get_balance(Chain, GasSymbol, ToAddr),
                                                                                    case ToGasBalance < GatherGas0 of
                                                                                        true ->
                                                                                            case lib_chain_addrs:get_addrs_by_type(Chain, ?ADDR_TYPE_FEE) of
                                                                                                [FeeAddr|_] -> % 手续费地址只有一个
                                                                                                    FeeGasBalance = lib_chain:get_balance(Chain, GasSymbol, FeeAddr),
                                                                                                    case FeeGasBalance < GatherGas0 of
                                                                                                        true ->
                                                                                                            throw({error, <<"gather gas not enough, chain=", Chain/binary, ", symbol=", Symbol/binary, ", addr=", FeeAddr/binary>>});
                                                                                                        false ->
                                                                                                            void
                                                                                                    end;
                                                                                                _ ->
                                                                                                    throw({error, <<"gather gas not enough, chain=", Chain/binary, ", symbol=", Symbol/binary, ", addr=", ToAddr/binary>>})
                                                                                            end;
                                                                                        false -> void
                                                                                    end,
                                                                                    {ToBalance0, GatherGas0}
                                                                            end,
                                                                        % 送往签名节点去生成签名的交易
                                                                        case lib_sign_node:make_tx(Chain, Symbol, GatherAddr, [{ToAddr,
                                                                                                                                case Symbol =:= GasSymbol of
                                                                                                                                    true -> ToBalance + GatherGas;
                                                                                                                                    false -> ToBalance
                                                                                                                                end}], GatherGas) of
                                                                            <<>> ->
                                                                                ?DBG("gather tx: Chain=~p, Symbol=~p, FromAddr=~p, GatherAddr=~p, Amount=~p, Gas~p~n",
                                                                                     [Chain, Symbol, ToAddr, GatherAddr, ToBalance, GatherGas]),
                                                                                throw({error, <<"empty sign">>});
                                                                            SignedTx when is_binary(SignedTx) ->
                                                                                % 发送交易
                                                                                ?DBG("signed gather tx: ~p~n", [SignedTx]),
                                                                                GatherTxId = lib_chain:send_tx(Chain, SignedTx),
                                                                                % 加入tx表，当周期扫描各交易的确认数达到预期后，发送通知给finance模块
                                                                                lib_tx_store:new_tx(Chain, GatherTxId, [0, 1, [ToAddr], GatherAddr, Symbol, ToBalance, 0, GatherGas, []], 1);
                                                                            Res ->
                                                                                throw(Res)
                                                                        end
                                                                end;
                                                            _ ->
                                                                throw({error, <<"no gather address found for chain: ", Chain/binary>>})
                                                        end,
                                                        % 直接加入tx表，因为前面逻辑已保证确认数是够的，当周期扫描各交易的确认数达到预期后，发送通知给finance模块（放后面是为了尽力保证成功处理后才登记在案）
                                                        lib_tx_store:new_tx(Chain, TxId, [0, 0, FromAddrList, ToAddr, Symbol, Amount, Gas, Gas, []]);
                                                    false ->
                                                        % 直接加入tx表，因为前面逻辑已保证确认数是够的，当周期扫描各交易的确认数达到预期后，发送通知给finance模块
                                                        lib_tx_store:new_tx(Chain, TxId, [0, 0, FromAddrList, ToAddr, Symbol, Amount, Gas, Gas, []]),
                                                        % 更新balance（形式上放后面是因为balance计算是依赖于tx_store的）
                                                        lib_chain_balances:update_balance(Chain, Symbol, ToAddr)
                                                end;
                                            false ->
                                                case lib_chain:need_gather(Chain) of
                                                    true ->
                                                        case lib_chain_addrs:is_addr_ours(Chain, ToAddr, ?ADDR_TYPE_GATHER) of
                                                            true ->
                                                                % 更新tx，当周期扫描各交易的确认数达到预期后，发送通知给finance模块
                                                                Tx = lib_tx_store:get_tx(Chain, TxId),
                                                                lib_tx_store:upd_tx(Chain, Tx#{<<"is_withdraw">> := 0}),
                                                                lib_chain_balances:update_balance(Chain, Symbol, ToAddr);
                                                            false -> void
                                                        end;
                                                    false -> void
                                                end
                                        end;
                                    _ ->
                                        % 已登记在案，说明已成功处理
                                        void
                                end
                            end,
                        [F(ToAddr) || ToAddr <- ToOurAddrList]
                end;
            _ -> void
        end,
        true
    catch
        _:Err ->
            ?ERR("do_tx error(chain=~p, tx_id=~p):~nerr_msg=~p~nstack=~p~n", [Chain, TxId0, Err, erlang:get_stacktrace()]),
            false
    end.

is_fee_charge(Chain, [FromAddr]) ->
    case lib_chain_addrs:get_addrs_by_type(Chain, ?ADDR_TYPE_FEE) of
        [FeeAddr|_] -> % 手续费地址只有一个
            FromAddr =:= FeeAddr;
        _ ->
            false
    end;
is_fee_charge(_Chain, _FromAddrList) ->
    false.

