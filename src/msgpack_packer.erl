%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%

-module(msgpack_packer).

-export([pack/2]).

-include("msgpack.hrl").

%% pack them all
-spec pack(msgpack:object(), option()) -> binary().
pack(I, _) when is_integer(I) andalso I < 0 ->
    pack_int(I);
pack(I, _) when is_integer(I) ->
    pack_uint(I);
pack(F, _) when is_float(F) ->
    pack_double(F);
pack(nil, _) ->
    << 16#C0:8 >>;
pack(false, _) ->
    << 16#C2:8 >>;
pack(true, _) ->
    << 16#C3:8 >>;

pack(Bin, Opt) when is_binary(Bin) ->
    case Opt of
        #options_v2{enable_str=true} = Opt -> pack_raw2(Bin);
        #options_v2{enable_str=false} = Opt -> pack_raw(Bin);
        #options_v1{} = Opt -> pack_raw(Bin)
    end;

pack(Atom, #options_v2{allow_atom=pack} = Opt) when is_atom(Atom) ->
    pack(erlang:atom_to_binary(Atom, unicode), Opt);

%% jiffy interface
pack({Map}, Opt = ?OPTION{interface=jiffy}) ->
    pack_map(Map, Opt);

%% jsx interface
pack(Map, Opt = ?OPTION{interface=jsx}) when Map =:= [{}]->
    pack_map([], Opt);
pack([{_,_}|_] = Map, Opt = ?OPTION{interface=jsx}) ->
    pack_map(Map, Opt);

pack(List, #options_v2{enable_str=true}=Opt)  when is_list(List) ->
    try
        case lists:all(fun is_integer/1, List) of
            true ->
                case pack_string(List, Opt) of
                    %% NOTE: due to erlang string format, msgpack can't
                    %% tell the differenec between string and list of
                    %% integers. Thus users have to take care not to
                    %% include invalid unicode characters.
                    %% Here to fallback into list(int()).
                    {error, _} -> pack_array(List, Opt);
                    Bin when is_binary(Bin) -> Bin
                end;
            false ->
                pack_array(List, Opt)
        end
    catch error:badarg -> pack_array(List, Opt)
    end;

pack(List, Opt)  when is_list(List) ->
    pack_array(List, Opt);

pack(Other, _) ->
    throw({badarg, Other}).

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

-spec pack_uint(non_neg_integer()) -> binary().
%% positive fixnum
pack_uint(N) when N < 128 ->
    << 2#0:1, N:7 >>;
%% uint 8
pack_uint(N) when (N band 16#FF) =:= N ->
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


-spec pack_double(float()) -> binary().
%% float : erlang's float is always IEEE 754 64bit format.
%% pack_float(F) when is_float(F)->
%%    << 16#CA:8, F:32/big-float-unit:1 >>.
%%    pack_double(F).
%% double
pack_double(F) ->
    << 16#CB:8, F:64/big-float-unit:1 >>.


-spec pack_raw(binary()) -> binary().
%% raw bytes in old spec
pack_raw(Bin) ->
    case byte_size(Bin) of
        Len when Len < 32->
            << 2#101:3, Len:5, Bin/binary >>;
        Len when Len < 16#10000 -> % 65536
            << 16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
        Len ->
            << 16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>
    end.

-spec pack_raw2(binary()) -> binary().
%% raw bytes in new spec
pack_raw2(Bin) ->
    case byte_size(Bin) of
        Len when Len < 32->
            << 16#C4:8, Len:8/big-unsigned-integer-unit:1, Bin/binary>>;
        Len when Len < 16#10000 -> % 65536
            << 16#C5:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
        Len ->
            << 16#C6:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>
    end.

%% @doc String MAY be unicode. Or may be EUC-JP, SJIS, UTF-1024 or anything.
%% EVERY implementation must show its binary length just after type indicator
%% to skip the damn string if its unreadable.
-spec pack_string(list(), msgpack_option()) -> binary() | {error, atom()}.
pack_string(String, _Opt) ->
    case unicode:characters_to_binary(String) of
        {error, _Bin, _} -> {error, broken_unicode};
        {incomplete, _Bin, _} -> {error, incomplete_unicode};
        Bin -> pack_raw(Bin)
    end.

-spec pack_array([msgpack:object()], list()) -> binary() | no_return().
pack_array([], _) ->
    << 2#1001:4, 0:4/integer-unit:1 >>;

pack_array([A], Opt) ->
    << 2#1001:4, 1:4/integer-unit:1, (pack(A, Opt))/binary >>;

pack_array([A, B], Opt) ->
    << 2#1001:4, 2:4/integer-unit:1, (pack(A, Opt))/binary, (pack(B, Opt))/binary >>;

pack_array([A, B, C], Opt) ->
    << 2#1001:4, 3:4/integer-unit:1, (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary >>;

pack_array([A, B, C, D], Opt) ->
    << 2#1001:4, 4:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary >>;

pack_array([A, B, C, D, E], Opt) ->
    << 2#1001:4, 5:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary >>;

pack_array([A, B, C, D, E, F], Opt) ->
    << 2#1001:4, 6:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G], Opt) ->
    << 2#1001:4, 7:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H], Opt) ->
    << 2#1001:4, 8:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I], Opt) ->
    << 2#1001:4, 9:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J], Opt) ->
    << 2#1001:4, 10:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K], Opt) ->
    << 2#1001:4, 11:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L], Opt) ->
    << 2#1001:4, 12:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M], Opt) ->
    << 2#1001:4, 13:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N], Opt) ->
    << 2#1001:4, 14:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary, (pack(N, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O], Opt) ->
    << 2#1001:4, 15:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary, (pack(N, Opt))/binary, (pack(O, Opt))/binary >>;

pack_array(L, Opt) ->
    case length(L) of
        Len when Len < 16#10000 -> % 65536
            <<16#DC:8, Len:16/big-unsigned-integer-unit:1, (<< <<(pack(E, Opt))/binary>> || E <- L >>)/binary>>;
        Len ->
            <<16#DD:8, Len:32/big-unsigned-integer-unit:1, (<< <<(pack(E, Opt))/binary>> || E <- L >>)/binary>>
    end.

-spec pack_map(msgpack:msgpack_map(), list(option())) -> binary() | no_return().
pack_map([{Ka, Va}], Opt)->
    << 2#1000:4, 1:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}], Opt)->
    << 2#1000:4, 2:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}], Opt)->
    << 2#1000:4, 3:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary,
       (pack(Kc, Opt))/binary, (pack(Vc, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}, {Kd, Vd}], Opt)->
    << 2#1000:4, 4:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary,
       (pack(Kc, Opt))/binary, (pack(Vc, Opt))/binary,
       (pack(Kd, Opt))/binary, (pack(Vd, Opt))/binary >>;

pack_map(M, Opt)->
    case length(M) of
        Len when Len < 16 ->
            <<2#1000:4, Len:4/integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>;
        Len when Len < 16#10000 -> % 65536
            <<16#DE:8, Len:16/big-unsigned-integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>;
        Len ->
            <<16#DF:8, Len:32/big-unsigned-integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>
    end.
