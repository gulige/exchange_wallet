%%%--------------------------------------
%%% @Module  : chain_ela
%%% @Description: 亦来云链的访问接口
%%%--------------------------------------
-module(chain_ela).
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

-define(CHAIN, <<"ela">>).
-define(SYMBOL, <<"ela">>).
-define(FACTOR, 100000000). % 10的8次方

-define(ERRNO_HTTP_REQ_FAILED, -1). % 请求失败
-define(ERRNO_HTTP_REQ_TIMEOUT, -2). % 请求超时

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).


start_node() ->
    ?INFO("starting elastos node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/ela; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/ela/stop.sh"),
    ok.

need_gather() ->
    % 类似比特币，可以多个地址一起转账，因此不需要余额汇总
    false.

get_new_address() ->
    {ok, #{<<"Address">> := Addr, <<"PrivateKey">> := PrivKey}} =
        http_sign_request([{method, <<"gen_priv_pub_addr">>},
                           {id, 0},
                           {params, []}]),
    {Addr, PrivKey}.

% 因为asset/balances/Addr不能指定确认数，所以通过utxos自己遍历一遍，来过滤求和
get_balance(?SYMBOL = Symbol, Addr) ->
    NeedConfirmCount = lib_chain_cfg:confirm_count(?CHAIN),
    ElaUTXOs = get_utxos(Symbol, Addr),
    F = fun(F, TxId, Amount) ->
            {ok, #{<<"vin">> := Vin, <<"confirmations">> := Confirmations}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [TxId, true]),
            case Confirmations >= NeedConfirmCount of
                true -> util:binary_to_float(Amount);
                false ->
                    L = lists:foldl(
                        fun(#{<<"txid">> := InTxId, <<"vout">> := InVoutIdx}, Acc) ->
                            {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
                            case [V || #{<<"address">> := InAddr, <<"value">> := V, <<"n">> := N} <- InVout, N =:= InVoutIdx, InAddr =:= Addr] of
                                [] -> Acc;
                                [InVal] -> [{InTxId, InVal} | Acc]
                            end
                        end, [], Vin),
                    lists:sum([F(F, FoundInTxId, FoundInVal) || {FoundInTxId, FoundInVal} <- L])
            end
        end,
    AmountL = [F(F, TxId, Amount) || #{<<"Txid">> := TxId, <<"Value">> := Amount} <- ElaUTXOs],
    lists:sum(AmountL);
get_balance(_Symbol, _Addr) ->
    0.

get_utxos(?SYMBOL, Addr) ->
    {ok, ResultL} = http_rest_request(<<"asset/utxos/", Addr/binary>>),
    case ResultL of
        null -> [];
        _ ->
            [ElaUTXOs] = [UTXOs || #{<<"AssetName">> := <<"ELA">>, <<"Utxo">> := UTXOs} <- ResultL],
            [UTXO || UTXO <- ElaUTXOs, has_no_utxo_lock(UTXO)]
    end;
get_utxos(_Symbol, _Addr) ->
    [].

has_no_utxo_lock(#{<<"Txid">> := TxId} = _UTXO) ->
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

make_tx(?SYMBOL = Symbol, ToAddr, AddrToDeltaAmountPairs, Gas) ->
    TotalAmount = lists:sum([DeltaAmount || {_, DeltaAmount} <- AddrToDeltaAmountPairs]),
    true = TotalAmount > Gas, % 相当于断言
    {UTXOInputsAccN, OutputsAccN, FailN} =
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
        true -> <<>>;
        false ->
            UTXOInputs = UTXOInputsAccN,
            Outputs = [make_output(ToAddr, TotalAmount - Gas) | OutputsAccN],
            ?DBG("UTXOInputs=~p~nOutputs=~p~n", [UTXOInputs, Outputs]),
            {ok, #{<<"rawTx">> := RawTx}} =
                http_sign_request([{method, <<"genRawTransaction">>},
                                   {id, 0},
                                   {params, [{[
                                               {
                                                <<"Transactions">>,
                                                [{[
                                                   {<<"UTXOInputs">>, UTXOInputs},
                                                   {<<"Outputs">>, Outputs}
                                                ]}]
                                               }
                                            ]}]
                                   }]),
            % 这里已签过名，不需要再调用sign_tx
            RawTx
    end;
make_tx(_Symbol, _ToAddr, _AddrToDeltaAmountPairs, _Gas) ->
    <<>>.

make_utxo_input(TxId, Index, Addr, PrivKey) ->
    {[{<<"txid">>, TxId}, {<<"index">>, Index}, {<<"privateKey">>, PrivKey}, {<<"address">>, Addr}]}.

make_output(Addr, Amount) ->
    case Amount == 0 of % 不要用=:=
        true -> void;
        false -> {[{<<"address">>, Addr}, {<<"amount">>, trunc(Amount * ?FACTOR)}]}
    end.

% 针对某个地址，返回{UTXOInputs, Output}，如果该地址选中的UTXOInputs是完整消耗无需找零的话，则Output设为void
select_outputs_greedy(Addr, Symbol, Amount) ->
    case lib_keystore:read_key(?CHAIN, Addr) of
        <<>> -> {[], void};
        PrivKey ->
            case get_utxos(Symbol, Addr) of
                [] -> {[], void};
                UTXOs ->
                    Lessers = lists:reverse(lists:sort([{Val, TxId, Index} || #{<<"Txid">> := TxId, <<"Index">> := Index, <<"Value">> := ValStr} <- UTXOs, (Val = util:binary_to_float(ValStr)) < Amount])),
                    Greaters = lists:sort([{Val, TxId, Index} || #{<<"Txid">> := TxId, <<"Index">> := Index, <<"Value">> := ValStr} <- UTXOs, (Val = util:binary_to_float(ValStr)) >= Amount]),
                    case Greaters of
                        [{MinGreaterVal, MinGreaterTxId, MinGreaterIndex}|_] ->
                            {[make_utxo_input(MinGreaterTxId, MinGreaterIndex, Addr, PrivKey)], make_output(Addr, MinGreaterVal - Amount)}; % 找零到原地址
                        [] ->
                            {UTXOInputsAccN, _AmountAccN, OutputAccN, DoneN} =
                                lists:foldl(fun({Val, TxId, Index}, {UTXOInputsAcc, AmountAcc, OutputAcc, Done}) ->
                                                case Done of
                                                    true -> {UTXOInputsAcc, AmountAcc, OutputAcc, Done};
                                                    false ->
                                                        NewAmountAcc = AmountAcc + Val,
                                                        case NewAmountAcc >= Amount of
                                                            true -> {[make_utxo_input(TxId, Index, Addr, PrivKey) | UTXOInputsAcc], NewAmountAcc, make_output(Addr, NewAmountAcc - Amount), true};
                                                            false -> {[make_utxo_input(TxId, Index, Addr, PrivKey) | UTXOInputsAcc], NewAmountAcc, OutputAcc, false}
                                                        end
                                                end
                                            end, {[], 0, void, false}, Lessers),
                            case DoneN of
                                true -> {UTXOInputsAccN, OutputAccN};
                                false -> {[], void}
                            end
                    end
            end
    end.

sign_tx(_Addr, _Tx) ->
    <<>>.

decode_tx(SignedTx) ->
    {ok, Res} =
        http_sign_request([{method, <<"decodeRawTransaction">>},
                           {id, 0},
                           {params, [{[
                                       {<<"RawTransaction">>, SignedTx}
                                    ]}]
                           }
                          ]),
    Res.

send_tx(SignedTx) ->
    {ok, Hash} =
        lib_chain:rpc_request(?CHAIN, sendrawtransaction, [SignedTx]),
    Hash.

get_latest_block_id() ->
    {ok, Hash} = lib_chain:rpc_request(?CHAIN, getbestblockhash, []),
    Hash.

get_block(BlockId) ->
    {ok, #{<<"height">> := Height, <<"confirmations">> := Confirmations, <<"tx">> := TxIds} = ResMap} =
        lib_chain:rpc_request(?CHAIN, getblock, [BlockId, 1]),
    NextBlockId = case maps:is_key(<<"nextblockhash">>, ResMap) of
                      true -> #{<<"nextblockhash">> := NBId} = ResMap, NBId;
                      false -> undefined
                  end,
    PrevBlockId = case maps:is_key(<<"previousblockhash">>, ResMap) of
                      true -> #{<<"previousblockhash">> := PBId} = ResMap, PBId;
                      false -> undefined
                  end,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

reverse_block_hash(BlockHash) ->
    << <<Byte:16>> || Byte <- lists:reverse([Byte || <<Byte:16>> <= BlockHash]) >>.

% 交易类型说明：
% CoinBase 0x00
% RegisterAsset 0x01
% TransferAsset 0x02
% Record 0x03
% Deploy 0x04
% SideMining 0x05
% IssueToken 0x06
% WithdrawAsset 0x07
% TransferCrossChainAsset 0x08
get_tx(TxId) ->
    case lib_chain:rpc_request(?CHAIN, getrawtransaction, [TxId, true]) of
        {ok, #{<<"type">> := TxType, <<"vin">> := Vin, <<"vout">> := Vout, <<"confirmations">> := Confirmations}} ->
            case TxType of
                2 ->
                    InAddrValPairs0 =
                        [begin
                             {ok, #{<<"vout">> := InVout}} = lib_chain:rpc_request(?CHAIN, getrawtransaction, [InTxId, true]),
                             [{InAddr, InVal}] = [{Addr, util:binary_to_float(Val)} || #{<<"address">> := Addr, <<"value">> := Val, <<"n">> := N} <- InVout, N =:= InVoutIdx],
                             {InAddr, InVal}
                         end || #{<<"txid">> := InTxId, <<"vout">> := InVoutIdx} <- Vin],
                    InAddrValPairs =
                        lists:foldl(fun({InAddr, InVal}, Acc) ->
                                        case lists:keyfind(InAddr, 1, Acc) of
                                            false -> [{InAddr, InVal} | Acc];
                                            {_, InValAcc} -> lists:keyreplace(InAddr, 1, Acc, {InAddr, InValAcc + InVal})
                                        end
                                    end, [], InAddrValPairs0),
                    {InAddrList, InValList} = lists:unzip(InAddrValPairs),
                    InTotalVal = lists:sum(InValList),
                    OutAddrValPairs0 = [{Addr, util:binary_to_float(Val)} || #{<<"address">> := Addr, <<"value">> := Val} <- Vout],
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
                    Gas = (InTotalVal * ?FACTOR - OutTotalVal * ?FACTOR - Change * ?FACTOR) / ?FACTOR,
                    {InAddrList, OutAddrList, ?SYMBOL, OutValList, Gas, Confirmations};
                _ ->
                    tx_type_we_dont_care
            end;
        {error, #{<<"code">> := 44001}} -> % cannot find transaction in blockchain and transactionpool
            undefined;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> undefined;
                false -> tx_type_we_dont_care
            end
    end.

import_priv_key(_PrivKey) ->
    unsupported.

get_http_rest_url() ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, jsonrpc_ela),
    {_, Url} = lists:keyfind(http_rest_host, 1, CfgList),
    <<"http://", Url/binary, "/api/v1/">>.

http_rest_request(ParamsUrl) ->
    Url0 = get_http_rest_url(),
    Url = <<Url0/binary, ParamsUrl/binary>>,
    ?DBG("Url: ~p~n", [Url]),
    case ibrowse:send_req(binary_to_list(Url), [], get) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case maps:get(<<"Error">>, JsonObject) of
                        0 -> % 成功
                            Data = maps:get(<<"Result">>, JsonObject),
                            {ok, Data};
                        ErrCode -> % 失败
                            {error, ErrCode, maps:get(<<"Desc">>, JsonObject)}
                    end;
                _ ->
                    throw({error, ?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

get_http_sign_url() ->
    App = case mod_disperse:server_type() of
              ?SVRTYPE_WALLET_NODE -> exchange_wallet;
              ?SVRTYPE_SIGN_NODE -> exchange_sign
          end,
    {ok, CfgList} = application:get_env(App, jsonrpc_ela),
    {_, Url} = lists:keyfind(http_sign_host, 1, CfgList),
    unicode:characters_to_list(<<"http://", Url/binary, "/">>).

http_sign_request(Params) ->
    Url = get_http_sign_url(),
    JsonParams = jiffy:encode({Params}),
    ?DBG("Url: ~p, JsonParams: ~p~n", [Url, JsonParams]),
    case ibrowse:send_req(Url, [?JSON_CONTENT], post, JsonParams) of
        {ok, Status, _Head, Body} ->
            case Status of
                "200" ->
                    ?DBG("Body: ~p~n", [Body]),
                    JsonObject = jiffy:decode(Body, [return_maps]),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case maps:get(<<"Desc">>, JsonObject) of
                        <<"SUCCESS">> -> % 成功
                            Data = maps:get(<<"Result">>, JsonObject),
                            {ok, Data};
                        ErrMsg -> % 失败
                            {error, ErrMsg, JsonObject}
                    end;
                _ ->
                    throw({error, ?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({error, ?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 以下是工具函数

make_tx_to_batch_send_ela() ->
    Symbol = ?SYMBOL,
    Gas = 0.000001,
    FromAddr = <<"EWcsTPJ6T7TNVw8nHkBypSNysRyiBEzerX">>,
    {ok, Bin} = file:read_file("./batch_send_addr_list.txt"),
    L = binary:split(Bin, [<<"\n">>], [global]),
    AddrToAmountPairs = [begin [Addr, Amount] = binary:split(One, [<<" ">>], [global]), {Addr, util:binary_to_float(Amount)} end || One <- L, One =/= <<>>],
    TotalAmount = util:round10d(lists:sum([Amount || {_, Amount} <- AddrToAmountPairs]) + Gas),
    {UTXOInputsAccN, OutputsAccN, FailN} =
        lists:foldl(fun({Addr, Amount}, {UTXOInputsAcc, OutputsAcc, Fail}) ->
                        case Fail of
                            true -> {[], [], true};
                            false ->
                                case select_outputs_greedy(Addr, Symbol, Amount) of
                                    {[], _} ->
                                        ?DBG("make_tx failed: UTXOInputsAcc=~p, OutputsAcc=~p, Addr=~p, Amount=~p~n", [UTXOInputsAcc, OutputsAcc, Addr, Amount]),
                                        {[], [], true};
                                    {UTXOInputsPerAddr, void} ->
                                        {UTXOInputsPerAddr ++ UTXOInputsAcc, OutputsAcc, Fail};
                                    {UTXOInputsPerAddr, OutputPerAddr} ->
                                        {UTXOInputsPerAddr ++ UTXOInputsAcc, [OutputPerAddr | OutputsAcc], Fail}
                                end
                        end
                    end, {[], [], false}, [{FromAddr, TotalAmount}]),
    case FailN of
        true -> <<>>;
        false ->
            UTXOInputs = UTXOInputsAccN,
            Outputs = OutputsAccN ++ [make_output(Addr, Amount) || {Addr, Amount} <- AddrToAmountPairs],
            ?DBG("UTXOInputs=~p~nOutputs=~p~n", [UTXOInputs, Outputs]),
            {ok, #{<<"rawTx">> := RawTx}} =
                http_sign_request([{method, <<"genRawTransaction">>},
                                   {id, 0},
                                   {params, [{[
                                               {
                                                <<"Transactions">>,
                                                [{[
                                                   {<<"UTXOInputs">>, UTXOInputs},
                                                   {<<"Outputs">>, Outputs}
                                                ]}]
                                               }
                                            ]}]
                                   }]),
            % 这里已签过名，不需要再调用sign_tx
            RawTx
    end.
