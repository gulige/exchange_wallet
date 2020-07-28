%%%--------------------------------------
%%% @Module  : chain_act
%%% @Description: Achain链的访问接口
%%%--------------------------------------
-module(chain_act).
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

-define(CHAIN, <<"act">>).
-define(SYMBOL, <<"act">>).
-define(FACTOR, 100000).


-define(WALLET_NAME, <<"game">>).
-define(PASSPHRASE, <<"xxx">>).


start_node() ->
    ?INFO("starting act node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/act; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/act/stop.sh"),
    ok.

need_gather() ->
    % 采用余额汇总流程（虽然可以采用子地址方式避免汇总，但那样需要添加新的流程和增加uuid生成代码）
    true.

%% ！！部署节点时需要执行
wallet_create() ->
    {ok, _} = lib_chain:rpc_request(?CHAIN, wallet_create, [?WALLET_NAME, ?PASSPHRASE]).

wallet_open() ->
    {ok, _} = lib_chain:rpc_request(?CHAIN, wallet_open, [?WALLET_NAME]).

wallet_passphrase(Seconds) ->
    {ok, _} = lib_chain:rpc_request(?CHAIN, wallet_unlock, [Seconds, ?PASSPHRASE]).

disable_automatic_backups() ->
    {ok, false} = lib_chain:rpc_request(?CHAIN, wallet_set_automatic_backups, [false]).

make_sure_usable() ->
    wallet_open(),
    wallet_passphrase(60),
    disable_automatic_backups(),
    ok.

set_address_to_account(Addr, Account) ->
    lib_mongo:write_one(?CHAIN, <<"act_address_to_account">>, #{<<"_id">> => Addr, <<"account">> => Account}),
    ok.

get_account_from_address(Addr) ->
    #{<<"account">> := Account} = lib_mongo:find_one(?CHAIN, <<"act_address_to_account">>, #{<<"_id">> => Addr}, #{<<"account">> => true}),
    Account.

% 批量生成地址前，解锁1小时：chain_act:wallet_passphrase(3600).
get_new_address() ->
    Account = <<(?WALLET_NAME)/binary, (integer_to_binary(util:distinct_longunixtime()))/binary>>,
    {ok, Addr} = lib_chain:rpc_request(?CHAIN, wallet_account_create, [Account]),
    ok = set_address_to_account(Addr, Account),
    PrivKey = dump_priv_key(Addr),
    {Addr, PrivKey}.

dump_priv_key(Addr) ->
    {ok, PrivKey} = lib_chain:rpc_request(?CHAIN, wallet_dump_private_key, [Addr]),
    PrivKey.

get_balance(?SYMBOL, Addr) ->
    {ok, L} = lib_chain:rpc_request(?CHAIN, blockchain_list_address_balances, [Addr]),
    BalanceL = [case is_binary(One) of
                    true -> util:binary_to_float(One) / ?FACTOR;
                    false -> One / ?FACTOR
                end || [_, #{<<"condition">> := #{<<"asset_id">> := 0}, <<"balance">> := One}] <- L],
    lists:sum(BalanceL);
get_balance(_Symbol, _Addr) ->
    0.

% ACT与多资产交易手续费内部固定是0.01ACT，合约交易大约为0.02ACT左右
make_tx(?SYMBOL, ToAddr, [{FromAddr, Amount}], Gas) ->
    FromAccount = get_account_from_address(FromAddr),
    {ok, Tx} = rpc_request(create_transfer_transaction, [float_to_binary(util:round10d(Amount - Gas), [{decimals, 5}, compact]), <<"ACT">>, FromAccount, ToAddr]),
    % 这里已签过名，不需要再调用sign_tx
    term_to_binary(Tx);
make_tx(_Symbol, _ToAddr, _AddrToDeltaAmountPairs, _Gas) ->
    <<>>.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx0) ->
    SignedTx = binary_to_term(SignedTx0),
    {ok, TxId} = lib_chain:rpc_request(?CHAIN, network_broadcast_transaction, [SignedTx]),
    TxId.

get_latest_block_id() ->
    {ok, Height} = lib_chain:rpc_request(?CHAIN, blockchain_get_block_count, []),
    {ok, #{<<"id">> := BlockId}} =
         lib_chain:rpc_request(?CHAIN, blockchain_get_block, [Height]),
    BlockId.

get_block(BlockId) ->
    {ok, #{<<"block_num">> := Height, <<"user_transaction_ids">> := TxIds, <<"previous">> := PrevBlockId}} =
        lib_chain:rpc_request(?CHAIN, blockchain_get_block, [BlockId]),
    {ok, TopHeight} = lib_chain:rpc_request(?CHAIN, blockchain_get_block_count, []),
    % 根据块号连续的前提，来确定确认数和后继区块
    Confirmations = TopHeight - Height + 1,
    NextBlockId =
        case lib_chain:rpc_request(?CHAIN, blockchain_get_block, [Height + 1]) of
            {ok, #{<<"block_num">> := NextHeight, <<"id">> := NextBlockId_}} ->
                NextHeight = Height + 1,
                NextBlockId_;
            _ -> undefined
        end,
    {Height, TxIds, PrevBlockId, NextBlockId, Confirmations}.

get_tx(TxId) ->
    case rpc_request(blockchain_get_pretty_transaction, [TxId]) of
        {ok, #{<<"block_num">> := Height,
               <<"fee">> := #{<<"asset_id">> := 0, <<"amount">> := Gas},
               <<"trx_type">> := 0,
               <<"ledger_entries">> := [#{<<"amount">> := #{<<"asset_id">> := 0, <<"amount">> := Amount0},
                                          <<"from_account">> := FromAddr,
                                          <<"to_account">> := ToAddr}]
              }
        } ->
            Amount = case is_binary(Amount0) of
                         true -> binary_to_integer(Amount0);
                         false -> Amount0
                     end,
            {ok, TopHeight} = lib_chain:rpc_request(?CHAIN, blockchain_get_block_count, []),
            Confirmations = TopHeight - Height + 1,
            {[FromAddr], [ToAddr], ?SYMBOL, [Amount / ?FACTOR], Gas / ?FACTOR, Confirmations};
        {error, #{<<"code">> := 20022}} -> % transaction not found
            undefined;
        {ok, #{} = M} ->
            case maps:size(M) =:= 0 of
                true -> undefined;
                false -> tx_type_we_dont_care
            end
    end.

import_priv_key(PrivKey) ->
    make_sure_usable(),
    Account = <<(?WALLET_NAME)/binary, (integer_to_binary(util:distinct_longunixtime()))/binary>>,
    {ok, _} = lib_chain:rpc_request(?CHAIN, wallet_import_private_key, [PrivKey, Account, true, false]),
    {ok, Addr0} = lib_chain:rpc_request(?CHAIN, wallet_get_account_public_address, [Account]),
    Addr = binary:replace(Addr0, <<"ffffffffffffffffffffffffffffffff">>, <<>>),
    ok = set_address_to_account(Addr, Account),
    ok.

rpc_request(M, Args) ->
    case lib_chain:rpc_request(?CHAIN, M, Args) of
        {error, #{<<"code">> := 0}} -> % closed
            make_sure_usable(),
            lib_chain:rpc_request(?CHAIN, M, Args);
        {error, #{<<"code">> := 10}} -> % locked
            make_sure_usable(),
            lib_chain:rpc_request(?CHAIN, M, Args);
        Res -> Res
    end.

