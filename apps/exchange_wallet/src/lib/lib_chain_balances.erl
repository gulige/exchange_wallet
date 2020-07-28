%%%--------------------------------------
%%% @Module  : lib_chain_balances
%%% @Description:某种链的地址的余额
%%%--------------------------------------
-module(lib_chain_balances).

-include_lib("exchange_common/include/common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/1,
        update_balance/3,
        find_addrs_with_fittest_balance/3]).

-export([get_total_balance_from_chain/2, get_total_balance_from_ets/2]).

-define(BALANCE, <<"balance">>).
-define(SORT_BALANCE, <<"sort_balance">>).

init(Chains) ->
    [load_balances(Chain) || Chain <- Chains],
    ok.

get_balance(Chain, Symbol, Addr) ->
    case catch lib_chain:get_balance(Chain, Symbol, Addr) of
        B when is_number(B) -> B;
        Err ->
            ?ERR("get_balance(addr=~s) error: ~p~n", [Addr, Err]),
            timer:sleep(1000),
            get_balance(Chain, Symbol, Addr)
    end.

%% load函数不需要像lib_chain_addrs那样export，因为地址就算动态生成了一批，但余额肯定是0，所以不用load
load_balances(<<"ttt">>) ->
    ok;
load_balances(<<"neo">>) ->
    ok;
load_balances(<<"eos">>) ->
    ok;
load_balances(Chain) ->
    Symbols = lib_chain:get_symbols_by_chain(Chain),
    ?INFO("chain balance loading: ~s, symbols=~p...~n", [Chain, Symbols]),
    % 先建ets表
    [begin
        ets:new(table_name(Chain, Symbol), [named_table, public, set, ?ETSRC]),
        ets:new(sort_table_name(Chain, Symbol), [named_table, public, ordered_set, ?ETSRC])
     end || Symbol <- Symbols],
    % 遍历地址，加载余额
    F = fun(Addr) ->
            [begin
                % 查询该地址的symbol余额
                ?DBG("load balance, addr=~s~n", [Addr]),
                Balance = get_balance(Chain, Symbol, Addr),
                case Balance > 0.00000001 of
                    true ->
                        ets:insert(table_name(Chain, Symbol), {Addr, Balance}),
                        % 排序：balance相同的按地址排序
                        ets:insert(sort_table_name(Chain, Symbol), {{Balance, Addr}});
                    false ->
                        % 太小的不加入表中
                        void
                end
             end || Symbol <- Symbols]
        end,
    NeedAddrType = case lib_chain:need_gather(Chain) of
                       true -> ?ADDR_TYPE_GATHER;
                       false -> ?ADDR_TYPE_NORMAL
                   end,
    lib_chain_addrs:traverse(Chain, NeedAddrType, F),
    ?INFO("chain balance loading done: ~s~n", [Chain]),
    ok.

update_balance(<<"ttt">>, _Symbol, _Addr) ->
    ok;
update_balance(<<"neo">>, _Symbol, _Addr) ->
    ok;
update_balance(<<"eos">>, _Symbol, _Addr) ->
    ok;
update_balance(Chain, Symbol, Addr) ->
    NeedAddrType = case lib_chain:need_gather(Chain) of
                       true -> ?ADDR_TYPE_GATHER;
                       false -> ?ADDR_TYPE_NORMAL
                   end,
    case lib_chain_addrs:is_addr_ours(Chain, Addr, NeedAddrType) of
        true ->
            Table = table_name(Chain, Symbol),
            SortTable = sort_table_name(Chain, Symbol),
            case ets:lookup(Table, Addr) of
                [] -> void;
                [{_, OldBalance}] ->
                    ets:delete(SortTable, {OldBalance, Addr})
            end,
            % ！注意：先设余额为-999999999，相当于置脏，当这个标记一直未更新时，再取一次余额即可
            ets:insert(Table, {Addr, -999999999}),
            NewBalance0 = lib_chain:get_balance(Chain, Symbol, Addr) -
                             lib_tx_store:get_total_withdrawing_balance(Chain, Symbol, Addr),
            NewBalance = util:round10d(NewBalance0),
            ets:insert(Table, {Addr, NewBalance}),
            ets:insert(SortTable, {{NewBalance, Addr}});
        false -> void
    end,
    ok.

refresh_dirty_balances(Chain, Symbol) ->
    Table = table_name(Chain, Symbol),
    MS = ets:fun2ms(fun({Addr, Balance}) when Balance =:= -999999999 -> Addr end),
    L = ets:select(Table, MS),
    [update_balance(Chain, Symbol, Addr) || Addr <- L],
    ok.

% 返回{addr, delta_amount}列表，失败返回[]
find_addrs_with_fittest_balance(<<"ttt">>, _Symbol, Amount) when Amount > 0 ->
    [{<<>>, Amount}];
find_addrs_with_fittest_balance(<<"neo">>, _Symbol, Amount) when Amount > 0 ->
    [{<<>>, Amount}];
find_addrs_with_fittest_balance(<<"eos">>, _Symbol, Amount) when Amount > 0 ->
    [{<<>>, Amount}];
find_addrs_with_fittest_balance(Chain, Symbol, Amount) when Amount > 0 ->
    refresh_dirty_balances(Chain, Symbol),
    SortTable = sort_table_name(Chain, Symbol),
    FindF = fun(FindF, Key, AccSum, AccKeys) ->
                case ets:prev(SortTable, Key) of
                    '$end_of_table' ->
                        % 凑不出来，余额不足
                        [];
                    {Amt, Addr} = PrevKey ->
                        NewAccSum = util:round10d(AccSum + Amt),
                        case NewAccSum >= Amount of
                            true ->
                                % 已凑成，反序，将最大量排在首位
                                lists:reverse([{Addr, util:round10d(Amount - AccSum)} | AccKeys]);
                            false ->
                                % 继续凑
                                FindF(FindF, PrevKey, NewAccSum, [{Addr, Amt} | AccKeys])
                        end
                end
            end,
    case ets:next(SortTable, {Amount, <<"0">>}) of
        '$end_of_table' ->
            FindF(FindF, {Amount, <<"z">>}, 0, []);
        {_Amt, Addr} ->
            % 有大的，直接凑成
            [{Addr, Amount}]
    end.

table_name(Chain, Symbol) when is_binary(Chain), is_binary(Symbol) ->
    binary_to_atom(<<(?BALANCE)/binary, "_", Chain/binary, "_", Symbol/binary>>, utf8).

sort_table_name(Chain, Symbol) when is_binary(Chain), is_binary(Symbol) ->
    binary_to_atom(<<(?SORT_BALANCE)/binary, "_", Chain/binary, "_", Symbol/binary>>, utf8).

get_total_balance_from_chain(Chain, Symbol) ->
    Table = binary_to_atom(<<"address_", Chain/binary>>, utf8),
    L = ets:tab2list(Table),
    lists:sum([get_balance(Chain, Symbol, Addr) || {Addr, _AddrType} <- L]).

get_total_balance_from_ets(Chain, Symbol) ->
    case lib_chain:need_gather(Chain) of
        true ->
            Table = binary_to_atom(<<"address_", Chain/binary>>, utf8),
            [[GatherAddr]] = ets:match(Table, {'$1', ?ADDR_TYPE_GATHER}),
            get_balance(Chain, Symbol, GatherAddr);
        false ->
            Table = table_name(Chain, Symbol),
            L = ets:tab2list(Table),
            lists:sum([Balance || {_Addr, Balance} <- L])
    end.

