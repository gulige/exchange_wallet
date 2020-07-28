%%%--------------------------------------
%%% @Module  : chain_asch@3u
%%% @Description: 阿希链侧链3u的访问接口
%%%--------------------------------------
-module(chain_asch@3u).
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

% 阿希侧链需要导出这个方法供chain_asch@common调用
-export([get_symbol/1]).

-include("common.hrl").

-define(CHAIN, <<"asch@3u">>).
-define(SYMBOL, <<"uso">>).

%-define(DAPPID, <<"1847c9d186b72963fff6fa8d5c9d26d534c32d4d65dc984e09b881ef71bd6409">>).
-define(DAPPID, <<"ubiquity">>).


start_node() ->
    ?INFO("starting asch@3u node, cwd=~p~n", [file:get_cwd()]),
    Res = os:cmd("dir0=`pwd`; cd ./apps/exchange_sign/priv/asch@3u; ./start.sh; cd $dir0"),
    ?INFO("~p~n", [Res]),
    timer:sleep(5000),
    ok.

stop_node() ->
    os:cmd("./priv/asch@3u/stop.sh"),
    ok.

need_gather() ->
    % 采用余额汇总流程
    true.

get_new_address() ->
    chain_asch@common:get_new_address(?CHAIN, ?DAPPID).

get_balance(?SYMBOL = Symbol, Addr) ->
    chain_asch@common:get_balance(?CHAIN, ?DAPPID, get_inner_symbol(Symbol), Addr);
get_balance(_Symbol, _Addr) ->
    0.

make_tx(?SYMBOL = Symbol, ToAddr, [{FromAddr, Amount}], Gas) ->
    chain_asch@common:make_tx(?CHAIN, ?DAPPID, get_inner_symbol(Symbol), ToAddr, [{FromAddr, Amount}], Gas);
make_tx(_Symbol, _ToAddr, _AddrToDeltaAmountPairs, _Gas) ->
    <<>>.

sign_tx(_Addr, Tx) ->
    Tx.

send_tx(SignedTx) ->
    chain_asch@common:send_tx(?CHAIN, ?DAPPID, SignedTx).

get_latest_block_id() ->
    chain_asch@common:get_latest_block_id(?CHAIN, ?DAPPID).

get_block(BlockId) ->
    chain_asch@common:get_block(?CHAIN, ?DAPPID, BlockId).

get_tx(TxId) ->
    chain_asch@common:get_tx(?CHAIN, ?DAPPID, TxId).

import_priv_key(_PrivKey) ->
    unsupported.

get_inner_symbol(?SYMBOL) -> <<"ubiquity.USO">>;
get_inner_symbol(_) -> <<>>.

get_symbol(<<"ubiquity.USO">>) -> ?SYMBOL;
get_symbol(_) -> <<>>.

get_http_url(IsPub) ->
    chain_asch@common:get_http_url(?CHAIN, ?DAPPID, IsPub).

http_request(ParamsUrl) ->
    chain_asch@common:http_request(?CHAIN, ?DAPPID, ParamsUrl, [], false).

http_request(ParamsUrl, Params) ->
    chain_asch@common:http_request(?CHAIN, ?DAPPID, ParamsUrl, Params, false).

http_request(ParamsUrl, Params, IsPub) when is_list(Params) ->
    chain_asch@common:http_request(?CHAIN, ?DAPPID, ParamsUrl, Params, IsPub).

