%%%--------------------------------------
%%% @Module  : chain_ipc
%%% @Description: 知产链的访问接口
%%%--------------------------------------
-module(chain_ipc).
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

-define(CHAIN, <<"ipc">>).
-define(SYMBOL, <<"ipc">>).
-define(FACTOR, 1).


start_node() ->
    ?INFO("starting ipc node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/ipc; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/ipc/stop.sh"),
    ok.

need_gather() ->
    % 类似比特币，可以多个地址一起转账，因此不需要余额汇总
    false.

get_new_address() ->
    {ok, Addr} = lib_chain:rpc_request(?CHAIN, getnewaddress, []),
    PrivKey = dump_priv_key(Addr),
    {Addr, PrivKey}.

dump_priv_key(Addr) ->
    {ok, PrivKey} = lib_chain:rpc_request(?CHAIN, dumpprivkey, [Addr]),
    PrivKey.

% 因为不能指定确认数，所以通过utxos自己遍历一遍，来过滤求和
get_balance(Symbol, Addr) ->
    NeedConfirmCount = lib_chain_cfg:confirm_count(?CHAIN),
    UTXOs = get_utxos(Symbol, Addr, true),
    F = fun(F, TxId, Amount) ->
            {ok, #{<<"vin">> := Vin, <<"confirmations">> := Confirmations}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [TxId, true]),
            case Confirmations >= NeedConfirmCount of
                true -> Amount;
                false ->
                    L = lists:foldl(
                        fun(#{<<"txid">> := InTxId, <<"vout">> := InVoutIdx}, Acc) ->
                            {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
                            VL = case Symbol of
                                     ?SYMBOL -> [V || #{<<"scriptPubKey">> := #{<<"addresses">> := [InAddr]}, <<"value">> := V, <<"n">> := N} <- InVout, N =:= InVoutIdx, InAddr =:= Addr];
                                     _ -> [util:binary_to_float(V) || #{<<"scriptPubKey">> := #{<<"addresses">> := [InAddr]}, <<"TokenValue">> := V, <<"n">> := N} <- InVout, N =:= InVoutIdx, InAddr =:= Addr]
                                 end,
                            case VL of
                                [] -> Acc;
                                [InVal] -> [{InTxId, InVal} | Acc]
                            end
                        end, [], Vin),
                    lists:sum([F(F, FoundInTxId, FoundInVal) || {FoundInTxId, FoundInVal} <- L])
            end
        end,
    AmountL = [F(F, TxId, Amount) || #{<<"txid">> := TxId, <<"amount">> := Amount} <- UTXOs],
    lists:sum(AmountL).

get_utxos(Symbol, Addr, IncludeUnsafe) ->
    IsPub = case mod_disperse:server_type() of
                ?SVRTYPE_SIGN_NODE -> true;
                _ -> false
            end,
    Method = case Symbol of
                 ?SYMBOL -> listunspent;
                 _ -> listunspentToken
             end,
    {ok, UTXOs0} = case IncludeUnsafe of
                       true ->
                           % include_unsafe这个参数已废弃，所以无效，这里只能拿到最少8个确认数的utxo
                           lib_chain:rpc_request(?CHAIN, Method, [0, 9999999, [Addr], true], IsPub);
                       false ->
                           lib_chain:rpc_request(?CHAIN, Method, [lib_chain_cfg:confirm_count(?CHAIN), 9999999, [Addr]], IsPub)
                   end,
    UTXOs = case Symbol of
                ?SYMBOL -> UTXOs0;
                _ ->
                    % 将tokenvalue设到amount上，方便外层逻辑统一处理
                    [One#{<<"amount">> := TV} || (#{<<"TokenSymbol">> := TS, <<"tokenvalue">> := TV} = One) <- UTXOs0,
                                                 list_to_binary(string:to_lower(binary_to_list(TS))) =:= Symbol]
            end,
    %[UTXO || UTXO <- UTXOs, has_no_utxo_lock(UTXO)];
    UTXOs.

has_no_utxo_lock(#{<<"txid">> := TxId} = _UTXO) ->
    {ok, #{<<"vin">> := Vin, <<"locktime">> := LockTime}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [TxId, true]),
    L = [begin
             {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
             [OL] = [OutputLock || #{<<"outputlock">> := OutputLock, <<"n">> := N} <- InVout, N =:= InVoutIdx],
             {OL, Seq}
         end || #{<<"sequence">> := Seq, <<"txid">> := InTxId, <<"vout">> := InVoutIdx} <- Vin],
    case [Seq || {OL, Seq} <- L, OL > 0] of
        [] -> true;
        L2 ->
            case lists:any(fun(Seq) -> Seq =/= 16#FFFFFFFE end, L2) of
                true -> false;
                false -> not lists:any(fun({OL, _}) -> OL > LockTime end, L)
            end
    end.

make_tx(Symbol, ToAddr, AddrToDeltaAmountPairs0, Gas) ->
    AddrToDeltaAmountPairs = [{Addr, DeltaAmount} || {Addr, DeltaAmount} <- AddrToDeltaAmountPairs0, DeltaAmount > 0],
    TotalAmount = lists:sum([DeltaAmount || {_, DeltaAmount} <- AddrToDeltaAmountPairs]),
    case Symbol of
        ?SYMBOL ->
            true = TotalAmount > Gas; % 相当于断言
        _ -> void
    end,
    {UTXOInputsAccN, _OutputsAccN, FailN} =
        lists:foldl(fun({Addr, DeltaAmount}, {UTXOInputsAcc, OutputsAcc, Fail}) ->
                        case Fail of
                            true -> {[], [], true};
                            false ->
                                case select_outputs_greedy(Addr, Symbol, DeltaAmount) of
                                    {[], _} ->
                                        ?DBG("make_tx failed: UTXOInputsAcc=~p, OutputsAcc=~p, Addr=~p, DeltaAmount=~p~n", [UTXOInputsAcc, OutputsAcc, Addr, DeltaAmount]),
                                        {[], [], true};
                                    {UTXOInputsPerAddr, void} ->
                                        {UTXOInputsPerAddr ++ UTXOInputsAcc, OutputsAcc, Fail};
                                    {UTXOInputsPerAddr, OutputPerAddr} ->
                                        {UTXOInputsPerAddr ++ UTXOInputsAcc, [OutputPerAddr | OutputsAcc], Fail}
                                end
                        end
                    end, {[], [], false}, AddrToDeltaAmountPairs),
    case FailN of
        true -> throw({error, <<"select_outputs_greedy failed">>});
        false ->
            UTXOInputs = case Symbol of
                             ?SYMBOL -> UTXOInputsAccN;
                             _ ->
                                 AddrToDeltaAmountPairsGas = [{Addr, -DeltaAmount} || {Addr, DeltaAmount} <- AddrToDeltaAmountPairs0, DeltaAmount < 0],
                                 {UTXOInputsAccNGas, _OutputsAccNGas, FailNGas} =
                                     lists:foldl(fun({Addr, DeltaAmount}, {UTXOInputsAcc, OutputsAcc, Fail}) ->
                                                     case Fail of
                                                         true -> {[], [], true};
                                                         false ->
                                                             case select_outputs_greedy(Addr, ?SYMBOL, DeltaAmount) of
                                                                 {[], _} ->
                                                                     ?DBG("make_tx(gas) failed: UTXOInputsAcc=~p, OutputsAcc=~p, Addr=~p, DeltaAmount=~p~n", [UTXOInputsAcc, OutputsAcc, Addr, DeltaAmount]),
                                                                     {[], [], true};
                                                                 {UTXOInputsPerAddr, void} ->
                                                                     {UTXOInputsPerAddr ++ UTXOInputsAcc, OutputsAcc, Fail};
                                                                 {UTXOInputsPerAddr, OutputPerAddr} ->
                                                                     {UTXOInputsPerAddr ++ UTXOInputsAcc, [OutputPerAddr | OutputsAcc], Fail}
                                                             end
                                                     end
                                                 end, {[], [], false}, AddrToDeltaAmountPairsGas),
                                 case FailNGas of
                                     true -> throw({error, <<"select_outputs_greedy(gas) failed">>});
                                     false -> UTXOInputsAccN ++ UTXOInputsAccNGas
                                 end
                         end,
            Outputs = case Symbol of
                          ?SYMBOL -> make_output(ToAddr, TotalAmount - Gas); % ipc会自动计算找零
                          _ -> make_output(ToAddr, TotalAmount)
                      end,
            ?DBG("UTXOInputs=~p~nOutputs=~p~n", [UTXOInputs, Outputs]),
            {ok, RawTx} = lib_chain:rpc_request(?CHAIN, createrawtransactionForIsolation, [UTXOInputs, Outputs]),
            % 签名
            {ok, #{<<"complete">> := true, <<"hex">> := SignedRawTx}} =
                lib_chain:rpc_request(?CHAIN, signrawtransaction, [RawTx, UTXOInputs]),
            SignedRawTx
    end.

make_utxo_input(?SYMBOL, #{<<"txid">> := TxId, <<"vout">> := Index, <<"amount">> := Amount, <<"scriptPubKey">> := ScriptPubKey}) ->
    {[{<<"txid">>, TxId}, {<<"vout">>, Index}, {<<"scriptPubKey">>, ScriptPubKey}, {<<"amount">>, Amount}]};
make_utxo_input(Symbol, #{<<"TokenSymbol">> := TS, <<"TokenAccuracy">> := Accuracy, <<"txid">> := TxId, <<"vout">> := Index, <<"amount">> := Amount, <<"scriptPubKey">> := ScriptPubKey}) ->
    Symbol = list_to_binary(string:to_lower(binary_to_list(TS))), % 断言
    {[{<<"symbol">>, TS}, {<<"accuracy">>, Accuracy}, {<<"txid">>, TxId}, {<<"vout">>, Index}, {<<"scriptPubKey">>, ScriptPubKey}, {<<"amount">>, Amount}]}.

make_output(Addr, Amount) ->
    case Amount == 0 of % 不要用=:=
        true -> void;
        false -> {[{Addr, util:round10d(Amount * ?FACTOR)}]}
    end.

% 针对某个地址，返回{UTXOInputs, Output}，如果该地址选中的UTXOInputs是完整消耗无需找零的话，则Output设为void
select_outputs_greedy(Addr, Symbol, Amount) ->
    case get_utxos(Symbol, Addr, false) of
        [] -> {[], void};
        UTXOs ->
            Lessers = lists:reverse(lists:sort([{Val, One} || (#{<<"amount">> := Val} = One) <- UTXOs, Val < Amount])),
            Greaters = lists:sort([{Val, One} || (#{<<"amount">> := Val} = One) <- UTXOs, Val >= Amount]),
            case Greaters of
                [{MinGreaterVal, MinOne}|_] ->
                    {[make_utxo_input(Symbol, MinOne)], make_output(Addr, MinGreaterVal - Amount)}; % 找零到原地址
                [] ->
                    {UTXOInputsAccN, _AmountAccN, OutputAccN, DoneN} =
                        lists:foldl(fun({Val, One}, {UTXOInputsAcc, AmountAcc, OutputAcc, Done}) ->
                                        case Done of
                                            true -> {UTXOInputsAcc, AmountAcc, OutputAcc, Done};
                                            false ->
                                                NewAmountAcc = AmountAcc + Val,
                                                case NewAmountAcc >= Amount of
                                                    true -> {[make_utxo_input(Symbol, One) | UTXOInputsAcc], NewAmountAcc, make_output(Addr, NewAmountAcc - Amount), true};
                                                    false -> {[make_utxo_input(Symbol, One) | UTXOInputsAcc], NewAmountAcc, OutputAcc, false}
                                                end
                                        end
                                    end, {[], 0, void, false}, Lessers),
                    case DoneN of
                        true -> {UTXOInputsAccN, OutputAccN};
                        false -> {[], void}
                    end
            end
    end.

sign_tx(_Addr, _Tx) ->
    <<>>.

send_tx(SignedTx) ->
    {ok, Hash} = lib_chain:rpc_request(?CHAIN, sendrawtransaction, [SignedTx]),
    Hash.

get_latest_block_id() ->
    {ok, Height} = lib_chain:rpc_request(?CHAIN, getblockcount, []),
    {ok, Hash} = lib_chain:rpc_request(?CHAIN, getblockhash, [Height]),
    Hash.

get_block(BlockId) ->
    {ok, #{<<"height">> := Height, <<"confirmations">> := Confirmations, <<"tx">> := TxIds} = ResMap} =
        lib_chain:rpc_request(?CHAIN, getblock, [BlockId]),
    NextBlockId = case maps:is_key(<<"nextblockhash">>, ResMap) of
                      true -> #{<<"nextblockhash">> := NBId} = ResMap, NBId;
                      false -> undefined
                  end,
    PrevBlockId = case maps:is_key(<<"previousblockhash">>, ResMap) of
                      true -> #{<<"previousblockhash">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

get_tx(TxId) ->
    case lib_chain:rpc_request(?CHAIN, getrawtransaction, [TxId, true]) of
        {ok, #{<<"vin">> := Vin, <<"vout">> := Vout, <<"confirmations">> := Confirmations}} ->
            Symbol = get_tx_symbol(Vout),
            Symbols = lib_chain:get_symbols_by_chain(?CHAIN),
            case lists:member(Symbol, Symbols) of
                true ->
                    InAddrValPairs0 =
                        [begin
                             {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
                             case [{Addr, Val} || #{<<"scriptPubKey">> := #{<<"addresses">> := [Addr]}, <<"value">> := Val, <<"n">> := N} <- InVout, N =:= InVoutIdx] of
                                 [{InAddr, InVal}] -> {InAddr, InVal};
                                 [] -> void
                             end
                         end || #{<<"txid">> := InTxId, <<"vout">> := InVoutIdx} <- Vin],
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
                    OutAddrValPairs0 = [{Addr, Val} || #{<<"scriptPubKey">> := #{<<"addresses">> := [Addr]}, <<"value">> := Val} <- Vout],
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
                    Gas = util:round10d((InTotalVal * ?FACTOR - OutTotalVal * ?FACTOR - Change * ?FACTOR) / ?FACTOR),
                    case Symbol of
                        ?SYMBOL ->
                            {InAddrList, OutAddrList, Symbol, OutValList, Gas, Confirmations};
                        _ ->
                            InAddrValPairs0Token =
                                [begin
                                     {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
                                     case [{Addr, util:binary_to_float(Val)} || #{<<"scriptPubKey">> := #{<<"addresses">> := [Addr]}, <<"TokenValue">> := Val, <<"n">> := N} <- InVout, N =:= InVoutIdx] of
                                         [{InAddr, InVal}] -> {InAddr, InVal};
                                         [] -> void
                                     end
                                 end || #{<<"txid">> := InTxId, <<"vout">> := InVoutIdx} <- Vin],
                            InAddrValPairsToken =
                                lists:foldl(fun({InAddr, InVal}, Acc) ->
                                                case lists:keyfind(InAddr, 1, Acc) of
                                                    false -> [{InAddr, InVal} | Acc];
                                                    {_, InValAcc} -> lists:keyreplace(InAddr, 1, Acc, {InAddr, InValAcc + InVal})
                                                end;
                                               (_, Acc) -> Acc
                                            end, [], InAddrValPairs0Token),
                            {InAddrListToken, _} = lists:unzip(InAddrValPairsToken),
                            OutAddrValPairs0Token = [{Addr, util:binary_to_float(Val)} || #{<<"scriptPubKey">> := #{<<"addresses">> := [Addr]}, <<"TokenValue">> := Val} <- Vout],
                            {OutAddrValPairsToken, _ChangeToken} =
                                lists:foldl(fun({OutAddr, OutVal}, {OutAddrValPairsAcc, ChangeAcc}) ->
                                                case lists:member(OutAddr, InAddrList) of
                                                    true ->
                                                        {OutAddrValPairsAcc, ChangeAcc + OutVal};
                                                    false ->
                                                        case lists:keyfind(OutAddr, 1, OutAddrValPairsAcc) of
                                                            false -> {[{OutAddr, OutVal} | OutAddrValPairsAcc], ChangeAcc};
                                                            {_, OutValAcc} -> {lists:keyreplace(OutAddr, 1, OutAddrValPairsAcc, {OutAddr, OutValAcc + OutVal}), ChangeAcc}
                                                        end
                                                end
                                            end, {[], 0}, OutAddrValPairs0Token),
                            {OutAddrListToken, OutValListToken} = lists:unzip(OutAddrValPairsToken),
                            {InAddrListToken++(OutAddrList--InAddrListToken), OutAddrListToken, Symbol, OutValListToken, Gas, Confirmations}
                    end;
                false ->
                    symbol_we_dont_care
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> temporarily_not_found;
                false -> tx_type_we_dont_care
            end
    end.

get_tx_symbol(Vout) ->
    Types = [Type || #{<<"type">> := Type} <- Vout],
    case lists:member(5, Types) of
        true -> % 代币
            [#{<<"TokenSymbol">> := Symbol0} | _] = [One || (#{<<"type">> := Type} = One) <- Vout, Type =:= 5],
            list_to_binary(string:to_lower(binary_to_list(Symbol0)));
        false -> ?SYMBOL
    end.

import_priv_key(_PrivKey) ->
    unsupported.

