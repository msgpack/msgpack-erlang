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

-module(msgpack_proper).

-export([choose_type/0, choose_type_jsx/0, choose_type_jiffy/0]).

-export([array16_jsx/0, array32_jsx/0,
         map16_jsx/0, map32_jsx/0]).

-export([array16_jiffy/0, array32_jiffy/0,
         map16_jiffy/0, map32_jiffy/0]).


-include_lib("proper/include/proper.hrl").

%% default behaviour
choose_type() -> choose_type_jiffy().

choose_type_jsx() ->
    oneof([positive_fixnum(), negative_fixnum(),
           int8(), int16(), int32(), int64(),
           uint8(), uint16(), uint32(), uint64(),
           float(),
           nil(), boolean(),
           fix_raw(), raw16(), raw32(),
           fix_array_jsx(), %array16(), array32(),
           fix_map_jsx() %, map16(), map32()
          ]).


choose_type_jiffy() ->
    oneof([positive_fixnum(), negative_fixnum(),
           int8(), int16(), int32(), int64(),
           uint8(), uint16(), uint32(), uint64(),
           float(),
           nil(), boolean(),
           fix_raw(), raw16(), raw32(),
           fix_array_jiffy(), %array16(), array32(),
           fix_map_jiffy() %, map16(), map32()
          ]).

positive_fixnum() ->
    choose(0, 127).

negative_fixnum() ->
    choose(-32, -1).

int8() ->
    choose(-16#80, 0).

int16() ->
    choose(-16#8000, -16#80).

int32() ->
    choose(-16#80000000, -16#8000).

int64() ->
    choose(-16#8000000000000000, -16#80000000).

uint8() ->
    choose(0, 16#FF).

uint16() ->
    choose(16#FF, 16#FFFF).

uint32() ->
    choose(16#FFFF, 16#FFFFFFFF).

uint64() ->
    choose(16#FFFFFFFF, 16#FFFFFFFFFFFFFFFF).

nil() ->
    nil.

fix_raw() ->
    ?LET(Integer, choose(0, 31),
         ?LET(Binary, binary(Integer), Binary)).

raw16() ->
    ?LET(Integer, uint16(),
         ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Integer, uint32(),
         ?LET(Binary, binary(Integer), Binary)).

%% JSX
fix_array_jsx() ->
    ?LET(Integer, choose(0, 15),
         proper_gen:list_gen(Integer, choose_type_jsx())).

array16_jsx() ->
    proper_gen:list_gen(16, choose_type_jsx()).

array32_jsx() ->
    [ N || N <- lists:seq(0, 16#010000)].

fix_map_jsx() ->
    ?LET(Integer, choose(0, 15),
         proper_gen:list_gen(Integer, {choose_type_jsx(), choose_type_jsx()})).

mapempty_jsx() ->
    [{}].

map16_jsx() ->
    proper_gen:list_gen(16, {choose_type_jsx(), choose_type_jsx()}).

map32_jsx() ->
    [{N, N * N} || N <- lists:seq(0, 16#010000)].


%% Jiffy
fix_array_jiffy() ->
    ?LET(Integer, choose(0, 15),
         proper_gen:list_gen(Integer, choose_type_jiffy())).

array16_jiffy() ->
    proper_gen:list_gen(16, choose_type_jiffy()).

array32_jiffy() ->
    [ N || N <- lists:seq(0, 16#010000)].

fix_map_jiffy() ->
    ?LET(Integer, choose(0, 15),
         {proper_gen:list_gen(Integer, {choose_type_jiffy(), choose_type_jiffy()})}).

mapempty_jiffy() ->
    {[]}.

map16_jiffy() ->
    {proper_gen:list_gen(16, {choose_type_jiffy(), choose_type_jiffy()})}.

map32_jiffy() ->
    {[{N, N * N} || N <- lists:seq(0, 16#010000)]}.
