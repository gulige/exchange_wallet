%%%--------------------------------------
%%% @Module  : lib_keycrypto
%%% @Description:key加密
%%%--------------------------------------
-module(lib_keycrypto).

-include_lib("exchange_common/include/common.hrl").

-export([encrypt_key/1,
         decrypt_key/1,
         rand_password/0]).

-define(PASSWORD, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).


encrypt_key(Key) when is_binary(Key) ->
    P = ?PASSWORD,
    IV = crypto:strong_rand_bytes(16),
    Bin1 = pad(Key),
    Bin2 = crypto:block_encrypt(aes_cbc128, P, IV, Bin1),
    <<IV/binary, Bin2/binary>>.

decrypt_key(<<IV:16/binary, Bin1/binary>> = _Key) ->
    P = ?PASSWORD,
    Bin2 = crypto:block_decrypt(aes_cbc128, P, IV, Bin1),
    unpad(Bin2).

rand_password() -> erlang:md5(binary_to_list(crypto:strong_rand_bytes(10))).

pad(Bin) ->
    Extra = 16 - (size(Bin) rem 16),
    pad(Extra, Bin).

pad(0, Bin) ->
    %% have to add 15 random bytes and then a zero
    B1 = crypto:strong_rand_bytes(15),
    <<Bin/binary, B1/binary, 0>>;
pad(K, Bin) ->
    B1 = crypto:strong_rand_bytes(K - 1),
    <<Bin/binary, B1/binary, K>>.

unpad(B) ->
    Size = size(B),
    {_, B2} = split_binary(B, Size - 1),
    [Pad] = binary_to_list(B2),
    Len = case Pad of
              0 ->
                  %% the entire last block is padding
                  Size - 16;
              _ ->
                Size - Pad
          end,
    {Bfinal, _} = split_binary(B, Len),
    Bfinal.

