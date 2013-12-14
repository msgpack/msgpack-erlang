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

-module(msgpack_ext_example_tests).

-include_lib("eunit/include/eunit.hrl").

ext_test() ->
    Packer = fun({foobar, Me}, _) ->
                     {ok, {12, term_to_binary(Me)}}
             end,
    Unpacker = fun(12, Bin) ->
                       {ok, {foobar, binary_to_term(Bin)}}
               end,
    Ref = make_ref(),
    Opt = [{ext,{Packer,Unpacker}}],
    Bin = msgpack:pack({foobar, Ref}, Opt),
    {ok, {foobar, Ref}} = msgpack:unpack(Bin, Opt).
