-module(msgpack_gen).

-export([pack_uint/1, pack_int/1, pack_double/1, pack_raw/1]).

-spec pack_uint(non_neg_integer()) -> binary().
%% positive fixnum
pack_uint(N) when N < 128 ->
    << 2#0:1, N:7 >>;
%% uint 8
pack_uint(N) when (N band 16#FF) ->
    << 16#CC:8, N:8 >>;
%% uint 16
pack_uint(N) when (N band 16#FFFF) =:= N ->
    << 16#CD:8, N:16/big-unsigned-integer-unit:1 >>;
%% uint 32
pack_uint(N) when (N band 16#FFFFFFFF) =:= N->
    << 16#CE:8, N:32/big-unsigned-integer-unit:1 >>;
%% uint 64
pack_uint(N) ->
    << 16#CF:8, N:64/big-unsigned-integer-unit:1 >>.

-spec pack_int(integer()) -> binary().
%% negative fixnum
pack_int(N) when N >= -32->
    << 2#111:3, N:5 >>;
%% int 8
pack_int(N) when N > -128 ->
    << 16#D0:8, N:8/big-signed-integer-unit:1 >>;
%% int 16
pack_int(N) when N > -32768 ->
    << 16#D1:8, N:16/big-signed-integer-unit:1 >>;
%% int 32
pack_int(N) when (N band 16#FFFFFFFF) =:= N ->
    << 16#D2:8, N:32/big-signed-integer-unit:1 >>;
%% int 64
pack_int(N) ->
    << 16#D3:8, N:64/big-signed-integer-unit:1 >>.

-spec pack_double(float()) -> binary().
%% float : erlang's float is always IEEE 754 64bit format.
%% pack_float(F) when is_float(F)->
%%    << 16#CA:8, F:32/big-float-unit:1 >>.
%%    pack_double(F).
%% double
pack_double(F) ->
    << 16#CB:8, F:64/big-float-unit:1 >>.


-spec pack_raw(binary()) -> binary().
%% raw bytes
pack_raw(Bin) ->
    case byte_size(Bin) of
        Len when Len < 32->
            << 2#101:3, Len:5, Bin/binary >>;
        Len when Len < 16#10000 -> % 65536
            << 16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
        Len ->
            << 16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>
    end.
