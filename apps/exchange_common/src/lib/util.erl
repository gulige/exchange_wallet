%%%-----------------------------------
%%% @Module  : util
%%% @Description: 公共函数
%%%-----------------------------------
-module(util).
-export([
        term_to_bitstring/1,
        rand/2,
        round10d/1,
        ceil/1,
        floor/1,
        binary_to_float/1,
        pmap/2,
        traverse_ets/3,
        zero_if_negative/1,
        unixtime/0,
        longunixtime/0,
        distinct_longunixtime/0,
        timestamp_to_utc_string/1
	  ]
).

-include("common.hrl").

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
%%当列表中的值全小于255时，~p会显示字母，所以用~w，同时有个好处，对于字符串中含有单引号等字符时，不会导致sql语法错误
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Min > Max -> 0;
rand(Min, Max) ->
    %% 如果没有种子，将获取一个种子，以保证不同进程都可取得不同的种子
    case get(random_seed) of
        undefined ->
            <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
            RandSeed = {A, B, C},
            rand:seed(exsplus, RandSeed),
            put(random_seed, RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.

%%四舍五入保留8位小数
round10d(N) ->
    round(N*10000000000)/10000000000.

%%向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

binary_to_float(Bin) ->
    case catch erlang:binary_to_float(Bin) of
        {'EXIT',{badarg,_}} ->
            binary_to_integer(Bin);
        Float -> Float
    end.

pmap(F, L) ->
    Parent = self(),
    Ref = erlang:make_ref(),
    lists:foreach(fun(I) -> spawn(fun() -> Parent ! {Ref, (catch F(I))} end) end, L),
    gather(length(L), Ref, []).

gather(0, _, L) -> L;
gather(N, Ref, L) ->
    receive
        {Ref, Ret} -> gather(N-1, Ref, [Ret|L])
    end.

% 遍历ets表
traverse_ets(F, FilterF, Table) ->
    traverse_ets(F, FilterF, Table, ets:first(Table), 1).

traverse_ets(_F, _FilterF, _Table, '$end_of_table', _I) ->
    ok;
traverse_ets(F, FilterF, Table, Key, I) ->
    case ets:lookup(Table, Key) of
        [R] ->
            case is_function(FilterF) of
                true -> % 设了过滤函数
                    case FilterF(R) of
                        true -> % 不过滤掉
                            F(I, R),
                            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I+1);
                        false -> % 过滤掉
                            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I)
                    end;
                false -> % 没设过滤函数
                    F(I, R),
                    traverse_ets(F, FilterF, Table, ets:next(Table, Key), I+1)
                    end;
        [] ->
            traverse_ets(F, FilterF, Table, ets:next(Table, Key), I)
    end.

%% 如果为负，则取0
zero_if_negative(Num) ->
    if
        Num < 0 ->
            0;
        true ->
            Num
    end.

%% 取得当前的unix时间戳（单位为秒）
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 取得当前的unix时间戳（单位为毫秒）
longunixtime() ->
    {M, S, Ms} = os:timestamp(),
    M * 1000000000 + S * 1000 + Ms div 1000.

%% 取得当前的unix时间戳（单位为微秒）
distinct_longunixtime() ->
    {M, S, Ms} = erlang:now(),
    M * 1000000000000 + S * 1000000 + Ms.

timestamp_to_utc_string({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    S = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w", [Year, Month, Day, Hour, Minute, Second])),
    list_to_binary(S).

