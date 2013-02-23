-module(msgpack_jsx).

-export([unpack/1, pack/1]).

%% pack them all
-spec pack(msgpack:object()) -> binary() | no_return().
pack(I) when is_integer(I), I < 0 ->
    msgpack_gen:pack_int(I);
pack(I) when is_integer(I) ->
    msgpack_gen:pack_uint(I);
pack(F) when is_float(F) ->
    msgpack_gen:pack_double(F);
pack(nil) ->
    << 16#C0:8 >>;
pack(true) ->
    << 16#C3:8 >>;
pack(false) ->
    << 16#C2:8 >>;
pack(Bin) when is_binary(Bin) ->
    msgpack_gen:pack_raw(Bin);
pack([{}] = Map) ->
    pack_map(Map);
pack([{_,_} | _] = Map) ->
    pack_map(Map);
pack(List)  when is_list(List) ->
    pack_array(List);
pack(Other) ->
    throw({badarg, Other}).

%% unpack them all
-spec unpack(Bin::binary()) -> {msgpack:object(), binary()} | no_return().
%% ATOMS
unpack(<<16#C0, Rest/binary>>) ->
    {nil, Rest};
unpack(<<16#C2, Rest/binary>>) ->
    {false, Rest};
unpack(<<16#C3, Rest/binary>>) ->
    {true, Rest};

%% Floats
unpack(<<16#CA, V:32/float-unit:1, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#CB, V:64/float-unit:1, Rest/binary>>) ->
    {V, Rest};

%% Unsigned integers
unpack(<<16#CC, V:8/unsigned-integer, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#CD, V:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#CE, V:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#CF, V:64/big-unsigned-integer-unit:1, Rest/binary>>) ->
    {V, Rest};

%% Signed integers
unpack(<<16#D0, V:8/signed-integer, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#D1, V:16/big-signed-integer-unit:1, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#D2, V:32/big-signed-integer-unit:1, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#D3, V:64/big-signed-integer-unit:1, Rest/binary>>) ->
    {V, Rest};

%% Raw bytes
unpack(<<16#DA, L:16/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {V, Rest};
unpack(<<16#DB, L:32/unsigned-integer-unit:1, V:L/binary, Rest/binary>>) ->
    {V, Rest};

%% Arrays
unpack(<<16#DC, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
unpack(<<16#DD, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_array(Rest, L, []);
%% Maps
unpack(<<16#DE, L:16/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);
unpack(<<16#DF, L:32/big-unsigned-integer-unit:1, Rest/binary>>) ->
    unpack_map(Rest, L, []);

%% Tag-encoded lengths (kept last, for speed)
unpack(<<0:1, V:7, Rest/binary>>) ->
    {V, Rest};                  % positive int
unpack(<<2#111:3, V:5, Rest/binary>>) ->
    {V - 2#100000, Rest};       % negative int
unpack(<<2#101:3, L:5, V:L/binary, Rest/binary>>) ->
    {V, Rest};                  % raw bytes
unpack(<<2#1001:4, L:4, Rest/binary>>) ->
    unpack_array(Rest, L, []); % array
unpack(<<2#1000:4, L:4, Rest/binary>>) ->
    unpack_map(Rest, L, []);   % map

                                                % Invalid data
unpack(<<F, R/binary>>) when F==16#C1;
                              F==16#C4; F==16#C5; F==16#C6; F==16#C7; F==16#C8; F==16#C9;
                              F==16#D4; F==16#D5; F==16#D6; F==16#D7; F==16#D8; F==16#D9 ->
    throw({badarg, <<F, R/binary>>});
                                                % Incomplete data (we've covered every complete/invalid case; anything left is incomplete)
unpack(_) ->
    throw(incomplete).


-spec pack_array([msgpack:object()]) -> binary() | no_return().

%% list
pack_array([]) ->
    << 2#1001:4, 0:4/integer-unit:1 >>;

pack_array([A]) ->
    << 2#1001:4, 1:4/integer-unit:1, (pack(A))/binary >>;

pack_array([A, B]) ->
    << 2#1001:4, 2:4/integer-unit:1, (pack(A))/binary, (pack(B))/binary >>;

pack_array([A, B, C]) ->
    << 2#1001:4, 3:4/integer-unit:1, (pack(A))/binary, (pack(B))/binary, (pack(C))/binary >>;

pack_array([A, B, C, D]) ->
    << 2#1001:4, 4:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary >>;

pack_array([A, B, C, D, E]) ->
    << 2#1001:4, 5:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary >>;

pack_array([A, B, C, D, E, F]) ->
    << 2#1001:4, 6:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary >>;

pack_array([A, B, C, D, E, F, G]) ->
    << 2#1001:4, 7:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary >>;

pack_array([A, B, C, D, E, F, G, H]) ->
    << 2#1001:4, 8:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I]) ->
    << 2#1001:4, 9:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J]) ->
    << 2#1001:4, 10:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K]) ->
    << 2#1001:4, 11:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary, (pack(K))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L]) ->
    << 2#1001:4, 12:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary, (pack(K))/binary, (pack(L))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M]) ->
    << 2#1001:4, 13:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary, (pack(K))/binary, (pack(L))/binary,
       (pack(M))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N]) ->
    << 2#1001:4, 14:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary, (pack(K))/binary, (pack(L))/binary,
       (pack(M))/binary, (pack(N))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]) ->
    << 2#1001:4, 15:4/integer-unit:1,
       (pack(A))/binary, (pack(B))/binary, (pack(C))/binary, (pack(D))/binary,
       (pack(E))/binary, (pack(F))/binary, (pack(G))/binary, (pack(H))/binary,
       (pack(I))/binary, (pack(J))/binary, (pack(K))/binary, (pack(L))/binary,
       (pack(M))/binary, (pack(N))/binary, (pack(O))/binary >>;

pack_array(L) ->
    case length(L) of
        Len when Len < 16#10000 -> % 65536
            <<16#DC:8, Len:16/big-unsigned-integer-unit:1, (<< <<(pack(E))/binary>> || E <- L >>)/binary>>;
        Len ->
            <<16#DD:8, Len:32/big-unsigned-integer-unit:1, (<< <<(pack(E))/binary>> || E <- L >>)/binary>>
    end.

-spec unpack_array(binary(), non_neg_integer(), [msgpack:object()]) -> {[msgpack:object()], binary()} | no_return().
unpack_array(Bin, 0,   Acc) ->
    {lists:reverse(Acc), Bin};
unpack_array(Bin, Len, Acc) ->
    {Term, Rest} = unpack(Bin),
    unpack_array(Rest, Len-1, [Term|Acc]).

-spec pack_map(M::msgpack:msgpack_map()) -> binary() | no_return().
pack_map([{}])->
    << 2#1000:4, 0:4/integer-unit:1, <<>>/binary >>;

pack_map([{Ka, Va}])->
    << 2#1000:4, 1:4/integer-unit:1,
       (pack(Ka))/binary, (pack(Va))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}])->
    << 2#1000:4, 2:4/integer-unit:1,
       (pack(Ka))/binary, (pack(Va))/binary,
       (pack(Kb))/binary, (pack(Vb))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}])->
    << 2#1000:4, 3:4/integer-unit:1,
       (pack(Ka))/binary, (pack(Va))/binary,
       (pack(Kb))/binary, (pack(Vb))/binary,
       (pack(Kc))/binary, (pack(Vc))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}, {Kd, Vd}])->
    << 2#1000:4, 4:4/integer-unit:1,
       (pack(Ka))/binary, (pack(Va))/binary,
       (pack(Kb))/binary, (pack(Vb))/binary,
       (pack(Kc))/binary, (pack(Vc))/binary,
       (pack(Kd))/binary, (pack(Vd))/binary >>;

pack_map(M)->
    case length(M) of
        Len when Len < 16 ->
            <<2#1000:4, Len:4/integer-unit:1,
              (<< <<(pack(K))/binary, (pack(V))/binary>> || {K, V} <- M >>)/binary>>;
        Len when Len < 16#10000 -> % 65536
            <<16#DE:8, Len:16/big-unsigned-integer-unit:1,
              (<< <<(pack(K))/binary, (pack(V))/binary>> || {K, V} <- M >>)/binary>>;
        Len ->
            <<16#DF:8, Len:32/big-unsigned-integer-unit:1,
              (<< <<(pack(K))/binary, (pack(V))/binary>> || {K, V} <- M >>)/binary>>
    end.

                                                % Users SHOULD NOT send too long list: this uses lists:reverse/1
-spec unpack_map(binary(), non_neg_integer(), msgpack:msgpack_map()) ->
                         {msgpack:msgpack_map(), binary()} | no_return().
unpack_map(Bin, 0, []) ->
    {[{}], Bin};
unpack_map(Bin, 0,   Acc) ->
    {lists:reverse(Acc), Bin};
unpack_map(Bin, Len, Acc) ->
    {Key, Rest} = unpack(Bin),
    {Value, Rest2} = unpack(Rest),
    unpack_map(Rest2, Len-1, [{Key,Value}|Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
map_test()->
    Ints = lists:seq(0, 65),
    Map = [ {X, X*2} || X <- Ints ] ++ [{<<"hage">>, 324}, {43542, [nil, true, false]}],
    {ok, Map2} = msgpack:unpack(msgpack:pack(Map, [jsx]), [jsx]),
    ?assertEqual(Map, Map2),
    {ok, Empty} = msgpack:unpack(msgpack:pack([{}], [jsx]), [jsx]),
    ?assertEqual([{}], Empty),
    ok.
-endif.
