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

-export([pack/1, unpack/1, unpack_stream/1, pack/2, unpack/2, unpack_stream/2]).

-type msgpack_map_jsx() :: [{msgpack_term(), msgpack_term()}] | [{}].

-type msgpack_map_jiffy() :: {[{msgpack_term(), msgpack_term()}]}.

-type msgpack_map() :: msgpack_map_jsx() | msgpack_map_jiffy().

%% Erlang representation of msgpack data.
-type msgpack_term() :: [msgpack_term()] | msgpack_map_jsx() | msgpack_map_jiffy() | integer() | float() | binary().

%% for export
-export_type([object/0, msgpack_map/0]).
-type object() :: msgpack_term().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% external APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Encode an erlang term into an msgpack binary.
%%      Returns {error, {badarg, term()}} if the input is illegal.

-spec pack(Term::msgpack_term()) -> binary() | {error, {badarg, term()}}.
pack(Term) ->
    pack(Term, [jiffy]).

pack(Term, [jiffy])->
    try
        msgpack_jiffy:pack(Term)
    catch
        throw:Exception ->
            {error, Exception}
    end;

pack(Term, [jsx])->
    try
        msgpack_jsx:pack(Term)
    catch
        throw:Exception ->
            {error, Exception}
    end.

%% @doc Decode an msgpack binary into an erlang term.
%%      It only decodes the first msgpack packet contained in the binary; the rest is returned as is.
%%      Returns {error, {badarg, term()}} if the input is corrupted.
%%      Returns {error, incomplete} if the input is not a full msgpack packet (caller should gather more data and try again).

unpack_stream(D) ->
    unpack_stream(D, [jiffy]).

unpack(D) ->
    unpack(D, [jiffy]).

-spec unpack_stream(Bin::binary()) -> {msgpack_term(), binary()} | {error, incomplete} | {error, {badarg, term()}}.
unpack_stream(Bin, [jiffy]) when is_binary(Bin) ->
    try
        msgpack_jiffy:unpack(Bin)
    catch
        throw:Exception ->
            {error, Exception}
    end;
unpack_stream(Bin, [jsx]) when is_binary(Bin) ->
    try
        msgpack_jsx:unpack(Bin)
    catch
        throw:Exception ->
            {error, Exception}
    end;

unpack_stream(Other, _Opts) ->
    {error, {badarg, Other}}.

%%% @doc Decode an msgpack binary into an erlang terms.
%%%      It only decodes ONLY ONE msgpack packets contained in the binary. No packets should not remain.
%%%      Returns {error, {badarg, term()}} if the input is corrupted.
%%%      Returns {error, incomplete} if the input is not a full msgpack packet (caller should gather more data and try again).
-spec unpack(binary()) -> {ok, msgpack_term()}
                              | {error, not_just_binary} % a term deserilized, but binary remains
                              | {error, incomplete}      % too few binary to deserialize complete binary
                              | {error, {badarg, term()}}.
unpack(Data, Opts) when is_binary(Data) ->
    case unpack_stream(Data, Opts) of
        {error, _} = E -> E;
        {Term, <<>>} -> {ok, Term};
        {_, Binary} when is_binary(Binary) andalso byte_size(Binary) > 0 -> {error, not_just_binary}
    end;

unpack(Badarg, _Opts) ->
    {error, {badarg, Badarg}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
    MatchFun0 = fun(Term) ->
                        {ok, Term} = msgpack:unpack(msgpack:pack(Term)),
                        Term
                end,
    MatchFun1 = fun(Term) ->
                        {ok, Term} = msgpack_nif:unpack(msgpack_nif:pack(Term)),
                        Term
                end,
    Tests = lists:map(MatchFun0, Tests),
    Tests = lists:map(MatchFun1, Tests).

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


other_test()->
    ?assertEqual({error,incomplete},msgpack:unpack(<<>>)).

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
