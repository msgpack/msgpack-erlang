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

%% @doc <a href="http://msgpack.org/">MessagePack</a> codec for Erlang.
%%
%%      APIs are almost compatible with <a href="http://redmine.msgpack.org/projects/msgpack/wiki/QuickStartC">C API</a>
%%      except for buffering functions (both copying and zero-copying), which are unavailable.
%%
%%   <table border="1">
%%     <caption>Equivalence between Erlang and <a href="http://msgpack.sourceforge.jp/spec">Msgpack type</a> :</caption>
%%     <tr><th>    erlang    </th><th>                            msgpack                                      </th></tr>
%%     <tr><td> integer()    </td><td> pos_fixnum/neg_fixnum/uint8/uint16/uint32/uint64/int8/int16/int32/int64 </td></tr>
%%     <tr><td> float()      </td><td> float/double                                                            </td></tr>
%%     <tr><td> nil          </td><td> nil                                                                     </td></tr>
%%     <tr><td> boolean()    </td><td> boolean                                                                 </td></tr>
%%     <tr><td> binary()     </td><td> fix_raw/raw16/raw32                                                     </td></tr>
%%     <tr><td> list()       </td><td> fix_array/array16/array32                                               </td></tr>
%%     <tr><td> {proplist()} </td><td> fix_map/map16/map32                                                     </td></tr>
%%   </table>
%% @end

-module(msgpack).

-export([pack/1, unpack/1, unpack_stream/1]).

-type msgpack_map() :: {[{msgpack_term(), msgpack_term()}]}.

% Erlang representation of msgpack data.
-type msgpack_term() :: [msgpack_term()] | msgpack_map() | integer() | float() | binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% external APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% @doc Encode an erlang term into an msgpack binary.
%      Returns {error, {badarg, term()}} if the input is illegal.
-spec pack(Term::msgpack_term()) -> binary() | {error, {badarg, term()}}.
pack(Term)->
    try
	pack_(Term)
    catch
	throw:Exception ->
	    {error, Exception}
    end.

% @doc Decode an msgpack binary into an erlang term.
%      It only decodes the first msgpack packet contained in the binary; the rest is returned as is.
%      Returns {error, {badarg, term()}} if the input is corrupted.
%      Returns {error, incomplete} if the input is not a full msgpack packet (caller should gather more data and try again).
-spec unpack_stream(Bin::binary()) -> {msgpack_term(), binary()} | {error, incomplete} | {error, {badarg, term()}}.
unpack_stream(Bin) when is_binary(Bin) ->
    try
	unpack_(Bin)
    catch
	throw:Exception ->
	    {error, Exception}
    end;
unpack_stream(Other) ->
    {error, {badarg, Other}}.

% @doc Decode an msgpack binary into an erlang terms.
%      It only decodes ONLY ONE msgpack packets contained in the binary. No packets should not remain.
%      Returns {error, {badarg, term()}} if the input is corrupted.
%      Returns {error, incomplete} if the input is not a full msgpack packet (caller should gather more data and try again).
-spec unpack(binary()) -> {ok, msgpack_term()}
                              | {error, not_just_binary} % a term deserilized, but binary remains
                              | {error, incomplete}      % too few binary to deserialize complete binary
                              | {error, {badarg, term()}}.
unpack(Data) when is_binary(Data) ->
    case unpack_stream(Data) of
        {error, _} = E -> E;
        {Term, <<>>} -> {ok, Term};
        {_, Binary} when is_binary(Binary) andalso byte_size(Binary) > 0 -> {error, not_just_binary}
    end;
unpack(Badarg) ->
    {error, {badarg, Badarg}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pack them all
-spec pack_(msgpack_term()) -> binary() | no_return().
pack_(I) when is_integer(I) andalso I < 0 ->
    pack_int_(I);
pack_(I) when is_integer(I) ->
    pack_uint_(I);
pack_(F) when is_float(F) ->
    pack_double(F);
pack_(nil) ->
    << 16#C0:8 >>;
pack_(true) ->
    << 16#C3:8 >>;
pack_(false) ->
    << 16#C2:8 >>;
pack_(Bin) when is_binary(Bin) ->
    pack_raw(Bin);
pack_(List)  when is_list(List) ->
    pack_array(List);
pack_({Map}) when is_list(Map) ->
    pack_map(Map);
pack_(Other) ->
    throw({badarg, Other}).


-spec pack_uint_(non_neg_integer()) -> binary().
% positive fixnum
pack_uint_(N) when N < 128 ->
    << 2#0:1, N:7 >>;
% uint 8
pack_uint_(N) when N < 256 ->
    << 16#CC:8, N:8 >>;
% uint 16
pack_uint_(N) when N < 65536 ->
    << 16#CD:8, N:16/big-unsigned-integer-unit:1 >>;
% uint 32
pack_uint_(N) when N < 16#FFFFFFFF->
    << 16#CE:8, N:32/big-unsigned-integer-unit:1 >>;
% uint 64
pack_uint_(N) ->
    << 16#CF:8, N:64/big-unsigned-integer-unit:1 >>.

-spec pack_int_(integer()) -> binary().
% negative fixnum
pack_int_(N) when N >= -32->
    << 2#111:3, N:5 >>;
% int 8
pack_int_(N) when N > -128 ->
    << 16#D0:8, N:8/big-signed-integer-unit:1 >>;
% int 16
pack_int_(N) when N > -32768 ->
    << 16#D1:8, N:16/big-signed-integer-unit:1 >>;
% int 32
pack_int_(N) when N > -16#80000000 ->
    << 16#D2:8, N:32/big-signed-integer-unit:1 >>;
% int 64
pack_int_(N) ->
    << 16#D3:8, N:64/big-signed-integer-unit:1 >>.


-spec pack_double(float()) -> binary().
% float : erlang's float is always IEEE 754 64bit format.
% pack_float(F) when is_float(F)->
%    << 16#CA:8, F:32/big-float-unit:1 >>.
%    pack_double(F).
% double
pack_double(F) ->
    << 16#CB:8, F:64/big-float-unit:1 >>.


-spec pack_raw(binary()) -> binary().
% raw bytes
pack_raw(Bin) ->
    case byte_size(Bin) of
	Len when Len < 32->
	    << 2#101:3, Len:5, Bin/binary >>;
	Len when Len < 16#10000 -> % 65536
	    << 16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
	Len ->
	    << 16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>
    end.


-spec pack_array([msgpack_term()]) -> binary() | no_return().
% list
pack_array(L) ->
    case length(L) of
 	Len when Len < 16 ->
 	    << 2#1001:4, Len:4/integer-unit:1, (pack_array_(L, <<>>))/binary >>;
	Len when Len < 16#10000 -> % 65536
	    << 16#DC:8, Len:16/big-unsigned-integer-unit:1, (pack_array_(L, <<>>))/binary >>;
	Len ->
	    << 16#DD:8, Len:32/big-unsigned-integer-unit:1, (pack_array_(L, <<>>))/binary >>
    end.

pack_array_([], Acc) -> Acc;
pack_array_([Head|Tail], Acc) ->
    pack_array_(Tail, <<Acc/binary,  (pack_(Head))/binary>>).

% Users SHOULD NOT send too long list: this uses lists:reverse/1
-spec unpack_array_(binary(), non_neg_integer(), [msgpack_term()]) -> {[msgpack_term()], binary()} | no_return().
unpack_array_(Bin, 0,   Acc) -> {lists:reverse(Acc), Bin};
unpack_array_(Bin, Len, Acc) ->
    {Term, Rest} = unpack_(Bin),
    unpack_array_(Rest, Len-1, [Term|Acc]).


-spec pack_map(M::[{msgpack_term(),msgpack_term()}]) -> binary() | no_return().
pack_map(M)->
    case length(M) of
	Len when Len < 16 ->
	    << 2#1000:4, Len:4/integer-unit:1, (pack_map_(M, <<>>))/binary >>;
	Len when Len < 16#10000 -> % 65536
	    << 16#DE:8, Len:16/big-unsigned-integer-unit:1, (pack_map_(M, <<>>))/binary >>;
	Len ->
	    << 16#DF:8, Len:32/big-unsigned-integer-unit:1, (pack_map_(M, <<>>))/binary >>
    end.

pack_map_([], Acc) -> Acc;
pack_map_([{Key,Value}|Tail], Acc) ->
    pack_map_(Tail, << Acc/binary, (pack_(Key))/binary, (pack_(Value))/binary>>).

% Users SHOULD NOT send too long list: this uses lists:reverse/1
-spec unpack_map_(binary(), non_neg_integer(), [{msgpack_term(), msgpack_term()}]) ->
			 {{[{msgpack_term(), msgpack_term()}]}, binary()} | no_return().
unpack_map_(Bin, 0,   Acc) -> {{lists:reverse(Acc)}, Bin};
unpack_map_(Bin, Len, Acc) ->
    {Key, Rest} = unpack_(Bin),
    {Value, Rest2} = unpack_(Rest),
    unpack_map_(Rest2, Len-1, [{Key,Value}|Acc]).

% unpack them all
-spec unpack_(Bin::binary()) -> {msgpack_term(), binary()} | no_return().
unpack_(Bin) ->
    case Bin of
% ATOMS
	<<16#C0, Rest/binary>> -> {nil, Rest};
	<<16#C2, Rest/binary>> -> {false, Rest};
	<<16#C3, Rest/binary>> -> {true, Rest};
% Floats
	<<16#CA, V:32/float-unit:1, Rest/binary>> ->   {V, Rest};
	<<16#CB, V:64/float-unit:1, Rest/binary>> ->   {V, Rest};
% Unsigned integers
	<<16#CC, V:8/unsigned-integer, Rest/binary>> ->             {V, Rest};
	<<16#CD, V:16/big-unsigned-integer-unit:1, Rest/binary>> -> {V, Rest};
	<<16#CE, V:32/big-unsigned-integer-unit:1, Rest/binary>> -> {V, Rest};
	<<16#CF, V:64/big-unsigned-integer-unit:1, Rest/binary>> -> {V, Rest};
% Signed integers
	<<16#D0, V:8/signed-integer, Rest/binary>> ->             {V, Rest};
	<<16#D1, V:16/big-signed-integer-unit:1, Rest/binary>> -> {V, Rest};
	<<16#D2, V:32/big-signed-integer-unit:1, Rest/binary>> -> {V, Rest};
	<<16#D3, V:64/big-signed-integer-unit:1, Rest/binary>> -> {V, Rest};
% Raw bytes
	<<16#DA, L:16/unsigned-integer-unit:1, V:L/binary, Rest/binary>> -> {V, Rest};
	<<16#DB, L:32/unsigned-integer-unit:1, V:L/binary, Rest/binary>> -> {V, Rest};
% Arrays
	<<16#DC, L:16/big-unsigned-integer-unit:1, Rest/binary>> -> unpack_array_(Rest, L, []);
	<<16#DD, L:32/big-unsigned-integer-unit:1, Rest/binary>> -> unpack_array_(Rest, L, []);
% Maps
	<<16#DE, L:16/big-unsigned-integer-unit:1, Rest/binary>> -> unpack_map_(Rest, L, []);
	<<16#DF, L:32/big-unsigned-integer-unit:1, Rest/binary>> -> unpack_map_(Rest, L, []);

% Tag-encoded lengths (kept last, for speed)
	<<0:1, V:7, Rest/binary>> ->                 {V, Rest};                  % positive int
	<<2#111:3, V:5, Rest/binary>> ->             {V - 2#100000, Rest};       % negative int
	<<2#101:3, L:5, V:L/binary, Rest/binary>> -> {V, Rest};                  % raw bytes
	<<2#1001:4, L:4, Rest/binary>> ->            unpack_array_(Rest, L, []); % array
	<<2#1000:4, L:4, Rest/binary>> ->            unpack_map_(Rest, L, []);   % map

% Invalid data
	<<F, R/binary>> when F==16#C1;
	                     F==16#C4; F==16#C5; F==16#C6; F==16#C7; F==16#C8; F==16#C9;
	                     F==16#D4; F==16#D5; F==16#D6; F==16#D7; F==16#D8; F==16#D9 ->
	    throw({badarg, <<F, R/binary>>});
% Incomplete data (we've covered every complete/invalid case; anything left is incomplete)
	_ ->
	    throw(incomplete)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_([]) -> 0;
test_([Term|Rest])->
    Pack = msgpack:pack(Term),
    ?assertEqual({ok, Term}, msgpack:unpack( Pack )),
    1+test_(Rest).

test_data()->
    [true, false, nil,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     -16#80000001,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, nil, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42
    ].

basic_test()->
    Tests = test_data(),
    Passed = test_(Tests),
    Passed = length(Tests).

test_p(Len,Term,OrigBin,Len) ->
    {ok, Term}=msgpack:unpack(OrigBin);
test_p(I,_,OrigBin,Len) when I < Len->
    <<Bin:I/binary, _/binary>> = OrigBin,
    ?assertEqual({error,incomplete}, msgpack:unpack(Bin)).

partial_test()-> % error handling test.
    Term = lists:seq(0, 45),
    Bin=msgpack:pack(Term),
    BinLen = byte_size(Bin),
    [test_p(X, Term, Bin, BinLen) || X <- lists:seq(0,BinLen)].

long_test()->
    Longer = lists:seq(0, 655),
    {ok, Longer} = msgpack:unpack(msgpack:pack(Longer)).

map_test()->
    Ints = lists:seq(0, 65),
    Map = {[ {X, X*2} || X <- Ints ] ++ [{<<"hage">>, 324}, {43542, [nil, true, false]}]},
    {ok, Map2} = msgpack:unpack(msgpack:pack(Map)),
    ?assertEqual(Map, Map2),
    {ok, Empty} = msgpack:unpack(msgpack:pack({[]})),
    ?assertEqual({[]}, Empty),
    ok.

other_test()->
    ?assertEqual({error,incomplete},msgpack:unpack(<<>>)).

benchmark_test()->
    Data=[test_data() || _ <- lists:seq(0, 10000)],
    S=?debugTime("  serialize", msgpack:pack(Data)),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S)),
    ?debugFmt("for ~p KB test data.", [byte_size(S) div 1024]).

error_test()->
    ?assertEqual({error,{badarg, atom}}, msgpack:pack(atom)),
    Term = {"hoge", "hage", atom},
    ?assertEqual({error,{badarg, Term}}, msgpack:pack(Term)).

long_binary_test()->
    A = msgpack:pack(1),
    B = msgpack:pack(10),
    C = msgpack:pack(100),
    {1, Rem0} = msgpack:unpack_stream(<<A/binary, B/binary, C/binary>>),
    {10, Rem1} = msgpack:unpack_stream(Rem0),
    {100, _Rem2} = msgpack:unpack_stream(Rem1),
    ok.

-endif.
