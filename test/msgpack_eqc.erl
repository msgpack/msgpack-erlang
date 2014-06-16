%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2014 UENISHI Kota
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

-module(msgpack_eqc).


-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

eqc_test_() ->
    {spawn,
     [
      ?_assert(quickcheck(numtests(10, prop_msgpack())))
     ]}.


prop_msgpack() ->
    ?FORALL(Int, int(),
            begin
                ?debugVal(Int),
                Int =:= Int
            end).

-endif.
