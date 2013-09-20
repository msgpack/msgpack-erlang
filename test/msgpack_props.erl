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

-module(msgpack_props).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(msgpack_proper, [choose_type_jsx/0,
                         choose_type_jiffy/0]).

prop_type() ->
    numtests(128,
        ?FORALL({Term, EnableStr}, {choose_type_jsx(), boolean()},
                begin
                    Opt = [jsx, {enable_str, EnableStr}],
                    Binary = msgpack:pack(Term, Opt),
                    {ok, Term1} = msgpack:unpack(Binary, Opt),
                    Term =:= Term1
                end)),
    numtests(300,
        ?FORALL({Term, EnableStr}, {choose_type_jiffy(), boolean()},
                begin
                    Opt = [jiffy, {enable_str, EnableStr}],
                    Binary = msgpack:pack(Term, Opt),
                    {ok, Term1} = msgpack:unpack(Binary, Opt),
                    Term =:= Term1
                end)).


choose_reserved() ->
    oneof([16#C1,
           16#C7,
           16#C8,
           16#C9,
           16#D4,
           16#D5,
           16#D6,
           16#D7,
           16#D8]).

prop_reserved() ->
    numtests(300,
        ?FORALL(Type, choose_reserved(),
                begin
                    {error, {badarg, Type1}} = msgpack:unpack(Type),
                    Type =:= Type1
                end)).
