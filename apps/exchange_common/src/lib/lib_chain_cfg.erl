%%%--------------------------------------
%%% @Module  : lib_chain_cfg
%%% @Description:链的配置，可修改热更
%%%--------------------------------------
-module(lib_chain_cfg).

-export([gas/2, confirm_count/1]).


%% 链上转账的汽油费，返回{汽油量, 汽油单位}
gas(<<"wic">>, _Amount) -> {0.0002, <<"wic">>};
gas(<<"ela">>, _Amount) -> {0.000001, <<"ela">>};
gas(<<"ipc">>, _Amount) -> {0.01, <<"ipc">>};
gas(<<"act">>, _Amount) -> {0.01, <<"act">>};
gas(<<"ply">>, _Amount) -> {0.01, <<"ipc">>};
gas(<<"xas">>, _Amount) -> {0.1, <<"xas">>};
gas(<<"ttt">>, _Amount) -> {0.001, <<"ttt">>};
gas(<<"neo">>, _Amount) -> {0, <<"gas">>};
gas(<<"gas">>, _Amount) -> {0, <<"gas">>};
gas(<<"ssc">>, _Amount) -> {0.01, <<"ssc">>};
gas(<<"uso">>, _Amount) -> {0.1, <<"uso">>}; % 阿希侧链3u币
gas(<<"ens">>, _Amount) -> {0.1, <<"xas">>}; % 阿希侧链ens币，我们接主链上的资产
gas(<<"emt">>, _Amount) -> {0.1, <<"xas">>}; % 阿希侧链ema币，我们接主链上的资产
gas(<<"eos">>, _Amount) -> {0, <<"eos">>};
gas(<<"btm">>, _Amount) -> {0.01, <<"btm">>};
gas(_Symbol, _Amount) -> {0, <<>>}.


%% 链上转账的确认数
confirm_count(<<"wicc">>) -> 30;
confirm_count(<<"ela">>) -> 30;
confirm_count(<<"ipc">>) -> 30;
confirm_count(<<"act">>) -> 30; % Achain的确认数被封装在内部，外面不用关心
confirm_count(<<"asch">>) -> 30;
confirm_count(<<"ttt">>) -> 1; % 0 - pending, 1 - final
confirm_count(<<"neo">>) -> 30;
confirm_count(<<"ssc">>) -> 30; % SelfSell的确认数被封装在内部，外面不用关心
confirm_count(<<"asch@3u">>) -> 30;
confirm_count(<<"eos">>) -> 1;
confirm_count(<<"btm">>) -> 30;
confirm_count(_Chain) -> 30.

