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
-export([start/0, stop/0,
	 connect/3, close/1, call/3, call_async/3, join/2]).

% -type address() :: string()|atom()|inet:ip_address().
-type mprc() :: inet:socket().

%%====================================================================
%% API
%%====================================================================
-spec start()-> ok.
start()->
    ?MODULE=ets:new(?MODULE, [set,public,named_table]), ok.

-spec stop()-> ok.
stop()->
    true=ets:delete(?MODULE), ok.

-spec connect(gen_tcp:ip_address(), Port::(0..65535), Options::[term()])->
		     {ok, mprc()}|{error,Reason::term()}.
connect(Address, Port, Options)->
    _Options1 = parse_options(Options),
    gen_tcp:connect(Address, Port, [binary, {packet,0}, {active,false}]).

% synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
-spec call(mprc(), Method::atom(), Argv::list()) -> 
		  {ok, any()} | {error, {atom(), any()}}.
call(Sock, Method, Argv) when is_atom(Method), is_list(Argv) ->
    {ok,CallID}=call_async(Sock,Method,Argv),
    ?MODULE:join(Sock,CallID).

call_async(Sock,Method,Argv)->
    CallID = get_callid(),
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    case msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]) of
	{error, Reason}->
	    {error, Reason};
	Pack ->
	    ok=gen_tcp:send(Sock, Pack),
	    {ok,CallID}
    end.

-spec join(mprc(), integer() | [integer()]) -> term() | [term()] | {error, term()}.
join(Sock, CallIDs) when is_list(CallIDs)-> join_(Sock, CallIDs, []);
join(Sock, CallID)->
    {ok, PackedMsg}  = gen_tcp:recv(Sock, 0),
    {[?MP_TYPE_RESPONSE, CallID, Error, Retval], <<>>} = msgpack:unpack(PackedMsg),
    call_done(CallID),
    case {Error,Retval} of
	{nil,Result}-> Result;
	_Other ->      {error, {Error,Retval}}
    end.

join_(_Sock, [], Got)-> Got;
join_(Sock, Remain, Got)->
    {ok, PackedMsg}  = gen_tcp:recv(Sock, 0),
    {[?MP_TYPE_RESPONSE, CallID, Error, Retval], <<>>} = msgpack:unpack(PackedMsg),
    case lists:member(CallID, Remain) of
	true->
	    Remain0 = lists:delete(CallID, Remain),
	    case {Error,Retval} of
		{nil,Result}->
		    join_(Sock, Remain0, [Result|Got]);
		_Other -> 
		    join_(Sock, Remain0, [{error, {Error,Retval}}|Got])
	    end;
	false -> {error, {bad_future, CallID}}
    end.

-spec close(mprc()) -> ok|{error,term()}.		    
close(Client)-> gen_tcp:close(Client).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_callid()->
    Cand = random:uniform(16#FFFFFFFF),
    case ets:insert_new(?MODULE, {Cand,Cand}) of
	true -> Cand;
	false -> get_callid()
    end.

call_done(CallID)->
    true=ets:delete(?MODULE, CallID), ok.

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
