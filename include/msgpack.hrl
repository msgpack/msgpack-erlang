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

-type option() :: [jsx | jiffy | nif].

-record(options_v1, {
          interface = jiffy :: jiffy | jsx,
          map_unpack_fun = fun msgpack_unpacker:unpack_map_jiffy/4 :: fun(),
          impl = erlang     :: erlang | nif
         }).

-record(options_v2, {
          interface = jiffy :: jiffy | jsx,
          map_unpack_fun = fun msgpack_unpacker:unpack_map_jiffy/4 :: fun(),
          impl = erlang     :: erlang | nif,
          allow_atom = none :: none | pack %% allows atom when packing
         }).
-define(OPTION, #options_v2).
-type msgpack_option() :: #options_v2{}.
