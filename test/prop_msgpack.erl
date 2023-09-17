-module(prop_msgpack).
-include_lib("proper/include/proper.hrl").
-include("msgpack.hrl").


%%% Primitive Properties %%%
prop_uint() ->
    ?FORALL(
       UnsignedInt,
       oneof([positive_fixnum(), uint8(), uint16(), uint32(), uint64()]),
       pack_and_unpack(UnsignedInt)).

prop_int() ->
    ?FORALL(
       Int,
       oneof([positive_fixnum(), negative_fixnum(), int8(), int16(), int32(), int64()]),
       pack_and_unpack(Int)).

prop_binary() ->
    ?FORALL(
       Binary,
       oneof([fix_raw(), raw16(), raw32()]),
       pack_and_unpack(Binary)).

prop_float() ->
    ?FORALL(
       Float,
       proper_types:float(),
       pack_and_unpack(Float)).

prop_primitive() ->
    ?FORALL(
       PrimObj,
       oneof(primitive_types()),
       pack_and_unpack(PrimObj)).


%%% Helpers %%%
pack_and_unpack(Obj) ->
    Bin = msgpack:pack(Obj),
    {ok, Obj} = msgpack:unpack(Bin),
    is_binary(Bin).

%%% Generators %%%
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
