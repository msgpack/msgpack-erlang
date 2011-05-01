%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2010-2011 UENISHI Kota
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

%%% @doc
%%   easy client that can't recieve notify protocol.
%%  <code>
%%  sample()->
%%  %just as a syntax sugar for start_link
%%  %YourModule defines receiver-callback when notification came from server.
%%   {ok, Pid}=gen_msgpack_rpc:connect(Identifier, YourModule, [Address, Port], [tcp]),
%%   gen_msgpack_rpc:call(Identifier, somemethod, [1,2]), % returns {ok, 3}
%%   gen_msgpack_rpc:call_async(Identifier, somemethod, [1,2]),
%%   receive
%%       {ok, Answer} -> ok;% maybe 3
%%       _ -> error
%%   after 1024 -> timeout end
%%   gen_msgpack_rpc:close(Pid).
%%  </code>
%%% @end

-module(mprc).

-include("msgpack_rpc.hrl").

%% external API
-export([connect/3, close/1,
	 call/2, call_async/2
	]).

% -type address() :: string()|atom()|inet:ip_address().
-type mprc() :: inet:socket().

%%====================================================================
%% API
%%====================================================================
-spec connect(gen_tcp:ip_address(), Port::(0..65535), Options::[term()])->
		     {ok, mprc()}|{error,Reason::term()}.
connect(Address, Port, Options)->
    _Options1 = parse_options(Options),
    gen_tcp:connect(Address, Port, [binary, {packet,0}, {active,false}]).

% synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
-spec call(Method::atom(), Argv::list()) -> 
		  {ok, any()} | {error, {atom(), any()}}.
call(Method, Argv) when is_atom(Method), is_list(Argv) ->
    {ok, CallID}=call_async(Method,Argv),
    CallID,
    receive
	{ok, nil,Result }->     {ok, Result};
	{ok, ResCode,Result }-> {error,{ResCode, Result}};
	{error, Reason2}->      {error, Reason2};
	_Other ->               {error, {unknown, _Other}}
    end.

% TODO: write test code for call_async/3
% afterwards, the caller will receive the response {ok, ResCode, Result} as a message
%% -spec call_async(CallID::non_neg_integer(),
%% 		 Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
%% call_async(CallID, Method, Argv)->
%%     call_async(?SERVER, CallID, Method, Argv).

-spec call_async(Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
call_async(Method, Argv) when is_atom(Method), is_list(Argv)->
    CallID = 3,
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    case msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]) of
	{error, Reason}->
	    {error, Reason};
	_Pack ->
	    ng
%	    ok=gen_server:call(Client, {call, {CallID, Pid} ,Pack})
    end.

% TODO: write test code for cancellation
%% -spec cancel_async_call(CallID::non_neg_integer())->ok.
%% cancel_async_call(CallID)-> cancel_async_call(?SERVER, CallID).

-spec close(mprc()) -> ok|{error,term()}.		    
close(Client)-> gen_tcp:close(Client).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_options(_)->
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_test_()->
    {setup, local,
     fun()-> ok end,
     fun(_)-> ok end,
     [
%      fun()-> mprc:start(), mprc:stop() end
%      fun() -> ok end,
     ]
    }.

-endif.
