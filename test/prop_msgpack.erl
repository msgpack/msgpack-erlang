%% Copyright (C) 2009-2023 UENISHI Kota
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

-module(prop_msgpack).
-include_lib("proper/include/proper.hrl").
-include("msgpack.hrl").


%% -define(NUMTESTS, 16).
%% -define(QC_OUT(P),
%%         eqc:on_output(fun(Str, Args) ->
%%                               io:format(user, Str, Args) end, P)).
%% -define(_assertProp(S),
%%         {timeout, ?NUMTESTS * 10,
%%          ?_assert(quickcheck(numtests(?NUMTESTS, ?QC_OUT(S))))}).

%% eqc_test_() ->
%%     {inparallel,
%%      [
%%       ?_assertProp(prop_msgpack()),
%%       ?_assertProp(prop_msgpack([{format, jiffy}])),
%%       ?_assertProp(prop_msgpack([{format, jsx}]))
%%       ]}.

%%% Primitive Properties %%%
prop_uint() ->
    ?FORALL(
       {UnsignedInt, Opts},
       {oneof([positive_fixnum(), uint8(), uint16(), uint32(), uint64()]),
        stable_opts()},
       pack_and_unpack(UnsignedInt, Opts)).

prop_int() ->
    ?FORALL(
       {Int, Opts},
       {oneof([positive_fixnum(), negative_fixnum(), int8(), int16(), int32(), int64()]),
        stable_opts()},
       pack_and_unpack(Int, Opts)).

prop_binary() ->
    ?FORALL(
       {Binary, Opts},
       {oneof([fix_raw(), raw16(), raw32()]), stable_opts()},
       pack_and_unpack(Binary, Opts)).

prop_primitive() ->
    ?FORALL(
       {PrimObj, Opts},
       {oneof(primitive_types()), stable_opts()},
       pack_and_unpack(PrimObj, Opts)).


prop_array_primitive() ->
    ?FORALL(
       {Array, Opts},
       {oneof([fix_array_primitive(), array16_primitive()]), stable_opts()},
       pack_and_unpack(Array, Opts)).

prop_msgpack() ->
    ?FORALL({Obj, Opts},
            {msgpack_object(), stable_opts()},
            pack_and_unpack(Obj, Opts)).


%%% Helpers %%%
pack_and_unpack(Obj, Opts) ->
    Bin = msgpack:pack(Obj, Opts),
    {ok, Obj} = msgpack:unpack(Bin, Opts),
    is_binary(Bin).


%%% Generators %%%
stable_opts() ->
    % TODO: build property tests on options
    oneof([
           [],
           [{map_format, jiffy}],
           [{map_format, jsx}]
          ]).

null() -> null.

positive_fixnum() -> choose(0, 127).
negative_fixnum() -> choose(-32, -1).

int8() ->  choose(-16#80, 16#7F).
int16() -> oneof([choose(-16#8000, -16#81),
                  choose(16#80, 16#7FFF)]).
int32() -> oneof([choose(-16#80000000, -16#8001),
                  choose(16#10000, 16#7FFFFFFF)]).
int64() -> oneof([choose(-16#8000000000000000, -16#80000001),
                  choose(16#100000000, 16#7FFFFFFFFFFFFFFF)]).

uint8() ->  choose(0, 16#FF).
uint16() -> choose(16#100, 16#FFFF).
uint32() -> choose(16#10000, 16#FFFFFFFF).
uint64() -> choose(16#100000000, 16#FFFFFFFFFFFFFFFF).

fix_raw() ->
    ?LET(Integer, choose(0, 31),
         ?LET(Binary, binary(Integer), Binary)).

raw16() ->
    ?LET(Integer, uint16(),
         ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Binary, binary(65537), Binary).

primitive_types() ->
    [
     null(),
     positive_fixnum(), negative_fixnum(),
     int8(), int16(), int32(), int64(),
     uint8(), uint16(), uint32(), uint64(),
     proper_types:float(), proper_types:bool(),
     fix_raw(), raw16(), raw32()
    ].


container_types() ->
    [ fix_array_primitive(), array16_primitive() ].

fix_array_primitive() ->
    % up to 2^4-1
    resize(15, proper_types:list(oneof(primitive_types()))).

array16_primitive() ->
    % Up to 2^16-1, but for performance
    resize(128, proper_types:list(oneof(primitive_types()))).


%% TODO: add map
msgpack_object() ->
    oneof(container_types() ++ primitive_types()).
