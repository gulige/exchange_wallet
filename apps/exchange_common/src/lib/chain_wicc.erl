%%%--------------------------------------
%%% @Module  : chain_wicc
%%% @Description: 维基链的访问接口
%%%--------------------------------------
-module(chain_wicc).

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

-define(CHAIN, <<"wicc">>).
-define(SYMBOL, <<"wic">>).
-define(FACTOR, 100000000).
%-define(PASSPHRASE, <<"xxx">>).


start_node() ->
    ?INFO("starting wicc node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/wicc; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/wicc/stop.sh"),
    ok.

need_gather() ->
    % 采用余额汇总流程
    true.

%% 该接口调完后，coind会关闭，请重启（首次成功，再次调用会报错）
%encrypt_wallet() ->
%    {ok, #{<<"encrypt">> := true}} =
%        lib_chain:rpc_request(?CHAIN, encryptwallet, [?PASSPHRASE]).
%
%wallet_passphrase(Seconds) ->
%    {ok, #{<<"passphrase">> := true}} =
%        lib_chain:rpc_request(?CHAIN, walletpassphrase, [?PASSPHRASE, Seconds]).

get_new_address() ->
    {ok, #{<<"addr">> := Addr}} =
        lib_chain:rpc_request(?CHAIN, getnewaddress),
    PrivKey = dump_priv_key(Addr),
    {Addr, PrivKey}.

dump_priv_key(Addr) ->
    {ok, #{<<"privkey">> := PrivKey}} =
        lib_chain:rpc_request(?CHAIN, dumpprivkey, [Addr]),
    PrivKey.

get_balance(?SYMBOL, Addr) ->
    {ok, #{<<"balance">> := Balance}} =
        lib_chain:rpc_request(?CHAIN, getbalance, [Addr, lib_chain_cfg:confirm_count(?CHAIN)]),
    Balance;
get_balance(_Symbol, _Addr) ->
    0.

make_tx(?SYMBOL, ToAddr, [{FromAddr, Amount}], Gas) ->
    case lib_chain:rpc_request(?CHAIN, sendtoaddressraw, [Gas * ?FACTOR, trunc(util:round10d(Amount - Gas) * ?FACTOR), FromAddr, ToAddr]) of
        {ok, #{<<"rawtx">> := RawTx}} ->
            % 维基链在这里已签过名，不需要再调用sign_tx
            RawTx;
        {error, #{<<"code">> := -5, <<"message">> := <<"CKeyID is not registed ">>}} ->
            case get(registing) of
                undefined ->
                    % 激活一下（当前已在签名机上）
                    RegGas = 0.0002,
                    regist_account_tx(FromAddr, RegGas),
                    put(registing, true),
                    % 等15秒
                    timer:sleep(15000),
                    make_tx(?SYMBOL, ToAddr, [{FromAddr, Amount - RegGas}], Gas);
                _ ->
                    % 等15秒
                    timer:sleep(15000),
                    make_tx(?SYMBOL, ToAddr, [{FromAddr, Amount}], Gas)
            end
    end;
make_tx(_Symbol, _ToAddr, _AddrToDeltaAmountPairs, _Gas) ->
    <<>>.

regist_account_tx(Addr, Gas) ->
    {ok, #{<<"hash">> := Hash}} =
        lib_chain:rpc_request(?CHAIN, registaccounttx, [Addr, trunc(Gas * ?FACTOR)]),
    Hash.

sign_tx(Addr, Tx) ->
    {ok, Sign} = lib_chain:rpc_request(?CHAIN, signmessage, [Addr, Tx]),
    Sign.

send_tx(SignedTx) ->
    {ok, #{<<"hash">> := Hash}} =
        lib_chain:rpc_request(?CHAIN, submittx, [SignedTx]),
    Hash.

get_latest_block_id() ->
    {ok, Height} = lib_chain:rpc_request(?CHAIN, getblockcount, []),
    {ok, #{<<"hash">> := Hash}} =
         lib_chain:rpc_request(?CHAIN, getblockhash, [Height]),
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
    case lib_chain:rpc_request(?CHAIN, gettxdetail, [TxId]) of
        {ok, #{<<"txtype">> := TxType} = ResMap} ->
            case TxType of
                <<"COMMON_TX">> ->
                    #{<<"addr">> := FromAddr, <<"desaddr">> := ToAddr,
                      <<"money">> := Amount, <<"fees">> := Gas,
                      <<"blockhash">> := BlockId} = ResMap,
                    {_Height, _TxIds, _PrevBlockId, _NextBlockId, Confirmations} = get_block(BlockId),
                    {[FromAddr], [ToAddr], ?SYMBOL, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
                _ ->
                    tx_type_we_dont_care
            end;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> undefined;
                false -> tx_type_we_dont_care
            end
    end.

import_priv_key(PrivKey) ->
    {ok, _} = lib_chain:rpc_request(?CHAIN, importprivkey, [PrivKey]).

