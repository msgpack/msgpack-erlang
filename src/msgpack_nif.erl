%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
%%
%% Created : 17 Feb 2013
%%
-module(msgpack_nif).

-export([pack/1, unpack/1, unpack_stream/1]).

-spec pack(msgpack:object()) -> binary() | {error, {badarg, term()}}.
pack(_) ->
    throw(nif_not_loaded).

-spec unpack_stream(binary()) -> {msgpack:object(), binary()} | {error, incomplete} | {error, {badarg, term()}}.
unpack_stream(_) ->
    throw(nif_not_loaded).

-spec unpack(binary()) -> {ok, msgpack:object()}
                              | {error, not_just_binary} % a term deserilized, but binary remains
                              | {error, incomplete}      % too few binary to deserialize complete binary
                              | {error, {badarg, term()}}.
unpack(Data) ->
    msgpack:unpack(Data).
