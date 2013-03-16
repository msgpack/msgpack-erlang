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

-module(msgpack_unpacker).

-export([unpack_stream/2]).

-include("msgpack.hrl").


%% unpack them all
-spec unpack_stream(Bin::binary(), msgpack_option()) -> {msgpack:object(), binary()} | no_return().
%% ATOMS
unpack_stream(<<16#C0, Rest/binary>>, _) ->
    {nil, Rest};
unpack_stream(<<16#C2, Rest/binary>>, _) ->
    {false, Rest};
unpack_stream(<<16#C3, Rest/binary>>, _) ->
    {true, Rest};

%% Floats
unpack_stream(<<16#CA, V:32/float-unit:1, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#CB, V:64/float-unit:1, Rest/binary>>, _) ->
    {V, Rest};

%% Unsigned integers
unpack_stream(<<16#CC, V:8/unsigned-integer, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#CD, V:16/big-unsigned-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#CE, V:32/big-unsigned-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#CF, V:64/big-unsigned-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};

%% Signed integers
unpack_stream(<<16#D0, V:8/signed-integer, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#D1, V:16/big-signed-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#D2, V:32/big-signed-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#D3, V:64/big-signed-integer-unit:1, Rest/binary>>, _) ->
    {V, Rest};

%% Raw bytes
unpack_stream(<<16#DA, L:16/unsigned-integer-unit:1, V:L/binary, Rest/binary>>, _) ->
    {V, Rest};
unpack_stream(<<16#DB, L:32/unsigned-integer-unit:1, V:L/binary, Rest/binary>>, _) ->
    {V, Rest};

%% Arrays
unpack_stream(<<16#DC, L:16/big-unsigned-integer-unit:1, Rest/binary>>, Opt) ->
    unpack_array(Rest, L, [], Opt);
unpack_stream(<<16#DD, L:32/big-unsigned-integer-unit:1, Rest/binary>>, Opt) ->
    unpack_array(Rest, L, [], Opt);
%% Maps
unpack_stream(<<16#DE, L:16/big-unsigned-integer-unit:1, Rest/binary>>, Opt) ->
    case Opt?OPTION.interface of
        jiffy -> unpack_map_jiffy(Rest, L, [], Opt);
        jsx   -> unpack_map_jsx(Rest, L, [], Opt)
    end;
unpack_stream(<<16#DF, L:32/big-unsigned-integer-unit:1, Rest/binary>>, Opt) ->
    case Opt?OPTION.interface of
        jiffy -> unpack_map_jiffy(Rest, L, [], Opt);
        jsx   -> unpack_map_jsx(Rest, L, [], Opt)
    end;

%% Tag-encoded lengths (kept last, for speed)
unpack_stream(<<0:1, V:7, Rest/binary>>, _) ->
    {V, Rest};                  % positive int
unpack_stream(<<2#111:3, V:5, Rest/binary>>, _) ->
    {V - 2#100000, Rest};       % negative int
unpack_stream(<<2#101:3, L:5, V:L/binary, Rest/binary>>, _) ->
    {V, Rest};                  % raw bytes
unpack_stream(<<2#1001:4, L:4, Rest/binary>>, Opt) ->
    unpack_array(Rest, L, [], Opt); % array
unpack_stream(<<2#1000:4, L:4, Rest/binary>>, Opt) ->
    io:format("~p", [Opt]),
    case Opt?OPTION.interface of
        jiffy -> unpack_map_jiffy(Rest, L, [], Opt);
        jsx   -> unpack_map_jsx(Rest, L, [], Opt)
    end;

%% Invalid data
unpack_stream(<<F, R/binary>>, _) when F==16#C1;
                                       F==16#C4; F==16#C5; F==16#C6; F==16#C7; F==16#C8; F==16#C9;
                                       F==16#D4; F==16#D5; F==16#D6; F==16#D7; F==16#D8; F==16#D9 ->
    throw({badarg, <<F, R/binary>>});
%% Incomplete data (we've covered every complete/invalid case; anything left is incomplete)
unpack_stream(_, _) ->
    throw(incomplete).

-spec unpack_array(binary(), non_neg_integer(), [msgpack:object()], msgpack_option()) -> {[msgpack:object()], binary()} | no_return().
unpack_array(Bin, 0,   Acc, _) ->
    {lists:reverse(Acc), Bin};
unpack_array(Bin, Len, Acc, Opt) ->
    {Term, Rest} = unpack_stream(Bin, Opt),
    unpack_array(Rest, Len-1, [Term|Acc], Opt).

%% %% Users SHOULD NOT send too long list: this uses lists:reverse/1
%% -spec unpack_map(binary(), non_neg_integer(), msgpack:msgpack_map()) ->
%%                         {msgpack:msgpack_map(), binary()} | no_return().
unpack_map_jiffy(Bin, 0,   Acc, _) ->
    {{lists:reverse(Acc)}, Bin};
unpack_map_jiffy(Bin, Len, Acc, Opt) ->
    {Key, Rest} = unpack_stream(Bin, Opt),
    {Value, Rest2} = unpack_stream(Rest, Opt),
    unpack_map_jiffy(Rest2, Len-1, [{Key,Value}|Acc], Opt).

%% -spec unpack_map(binary(), non_neg_integer(), msgpack:msgpack_map()) ->
%%                          {msgpack:msgpack_map(), binary()} | no_return().
unpack_map_jsx(Bin, 0, [], _) ->
    {[{}], Bin};
unpack_map_jsx(Bin, 0,   Acc, _) ->
    {lists:reverse(Acc), Bin};
unpack_map_jsx(Bin, Len, Acc, Opt) ->
    {Key, Rest} = unpack_stream(Bin, Opt),
    {Value, Rest2} = unpack_stream(Rest, Opt),
    unpack_map_jsx(Rest2, Len-1, [{Key,Value}|Acc], Opt).
