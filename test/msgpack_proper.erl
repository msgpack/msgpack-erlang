-module(msgpack_proper).

-export([choose_type/0]).

-export([array16/0, array32/0,
         map16/0, map32/0]).
         

-include_lib("proper/include/proper.hrl").

choose_type() ->
    oneof([positive_fixnum(), negative_fixnum(),
           int8(), int16(), int32(), int64(),
           uint8(), uint16(), uint32(), uint64(),
           float(),
           nil(), boolean(),
           fix_raw(), raw16(), raw32(),
           fix_array(), %array16(), array32(),
           fix_map() %, map16(), map32()
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

fix_array() ->
    ?LET(Integer, choose(0, 15),
         proper_gen:list_gen(Integer, choose_type())).

array16() ->
     proper_gen:list_gen(16, choose_type()).

array32() ->
    [ N || N <- lists:seq(0, 16#010000)].

fix_map() ->
    ?LET(Integer, choose(0, 15),
         {proper_gen:list_gen(Integer, {choose_type(), choose_type()})}).

map16() ->
     proper_gen:list_gen(16, {choose_type(), choose_type()}).

map32() ->
    [{N, N * N} || N <- lists:seq(0, 16#010000)].

fix_raw() ->
    ?LET(Integer, choose(0, 31),
        ?LET(Binary, binary(Integer), Binary)).

raw16() ->
    ?LET(Integer, uint16(),
        ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Integer, uint32(),
        ?LET(Binary, binary(Integer), Binary)).
