%%%--------------------------------------
%%% @Module  : lib_keystore
%%% @Description:key存储（加密存储）
%%%--------------------------------------
-module(lib_keystore).

-include_lib("exchange_common/include/common.hrl").

-export([init/1,
        read_key/2,
        write_key/3]).

-export([check/1]).

-define(KEYSTORE, <<"keystore">>).

init(Chains) ->
    [ets:new(table_name(Chain), [named_table, public, set, ?ETSRC, ?ETSWC]) || Chain <- Chains],
    ok.

read_key(Chain, Addr) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    case ets:lookup(Table, Addr) of
        [] ->
            case lib_mongo_priv:find_one(Chain, TableBin, #{<<"address">> => Addr}, #{<<"priv_key">> => true}) of
                #{<<"priv_key">> := EncryptedPriKey} ->
                    ets:insert(Table, {Addr, EncryptedPriKey}),
                    lib_keycrypto:decrypt_key(EncryptedPriKey);
                _ ->
                    <<>>
            end;
        [{_, EncryptedPriKey}] ->
            lib_keycrypto:decrypt_key(EncryptedPriKey)
    end.

write_key(Chain, Addr, PriKey) ->
    Table = table_name(Chain),
    TableBin = atom_to_binary(Table, utf8),
    EncryptedPriKey = lib_keycrypto:encrypt_key(PriKey),
    PriKey = lib_keycrypto:decrypt_key(EncryptedPriKey), % 断言
    ets:insert(Table, {Addr, EncryptedPriKey}),
    {{true, _}, _} = lib_mongo_priv:write_one(Chain, TableBin, #{<<"address">> => Addr, <<"priv_key">> => EncryptedPriKey}),
    ok.

table_name(Chain) when is_binary(Chain) ->
    binary_to_atom(<<(?KEYSTORE)/binary, "_", Chain/binary>>, utf8).

check(Chain) ->
    {ok, File} = file:open(binary_to_list(Chain) ++ "_dirty_addresses", write),
    F = fun(#{<<"address">> := Addr, <<"priv_key">> := EncryptedPriKey}) ->
            case catch lib_keycrypto:decrypt_key(EncryptedPriKey) of
                R when is_binary(R) -> void;
                _ -> io:format(File, "~s~n", [Addr])
            end
        end,
    lib_mongo_priv:iterate(Chain, <<"keystore_", Chain/binary>>, #{}, #{<<"_id">> => false, <<"address">> => true, <<"priv_key">> => true}, F),
    file:close(File),
    ok.

