%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2010 UENISHI Kota
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

-module(sample_client).

-behaviour(gen_msgpack_rpc).

-export([init/1, handle_call/3, terminate/2, code_change/3]).
-export([got_notify/1]).

-record(state, {}).

got_notify(BinPid)->
    Pid = binary_to_term(BinPid),
    Pid ! got_notify.

init(_)->
    {ok, #state{}}.

handle_call(_Msg,_From,State)->
    {noreply, State}.

terminate(_Reason,_State)->
    ok.

code_change(_,State,_)->
    {ok,State}.
