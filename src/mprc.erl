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
%%   {ok, S}=mprc:connect(Address, Port, [tcp]),
%%   mprc:call(S, somemethod, [1,2]), % returns 3
%%   mprc:call_async(S, somemethod, [1,2]),
%%   receive
%%       {ok, Answer} -> ok;% maybe 3
%%       _ -> error
%%   after 1024 -> timeout end
%%   mprc:close(Pid).
%%  </code>
%%% @end

-module(mprc).

-include("msgpack_rpc.hrl").
-include_lib("eunit/include/eunit.hrl").

%% external API
-export([start/0, stop/0,
	 connect/3, close/1, call/3, call_async/3, join/2,
	 controlling_process/1, active_once/1, append_binary/2]).

-record(mprc, { s :: inet:socket(), carry = <<>> :: binary() }).

-type mprc() :: #mprc{}.

%%====================================================================
%% API
%%====================================================================
-spec start()-> ok.
start()->
    msgpack_util:start().

-spec stop()-> ok.
stop()->
    msgpack_util:stop().

-spec connect(gen_tcp:ip_address(), Port::(0..65535), Options::[term()])->
		     {ok, mprc()}|{error,Reason::term()}.
connect(Address, Port, Options)->
    _Options1 = parse_options(Options),
    {ok,S}=gen_tcp:connect(Address, Port, [binary, {packet,0}, {active,false}]),
    {ok, #mprc{s=S}}.

% synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
-spec call(mprc(), Method::atom(), Argv::list()) -> 
		  {ok, term()} | {error, {atom(), any()}}.
call(MPRC, Method, Argv) when is_atom(Method), is_list(Argv) ->
    {ok,CallID}=call_async(MPRC,Method,Argv),
    ?MODULE:join(MPRC,CallID).

call_async(MPRC,Method,Argv)->
    CallID = msgpack_util:get_callid(),
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    case msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]) of
	{error, Reason}->
	    {error, Reason};
	Pack ->
	    ok=gen_tcp:send(MPRC#mprc.s, Pack),
	    {ok,CallID}
    end.

-spec join(mprc(), integer() | [integer()]) -> {term(), mprc} | {[term()], mprc()} | {error, term()}.
join(MPRC, CallIDs) when is_list(CallIDs)-> join_(MPRC, CallIDs, []);
join(MPRC, CallID)-> 
    {[Term], MPRC0} = join_(MPRC, [CallID], []),
    {Term, MPRC0}.

join_(MPRC, [], Got)-> {Got, MPRC};
join_(MPRC, [CallID|Remain], Got) when byte_size(MPRC#mprc.carry) > 0 ->
    case msgpack_util:lookup(CallID) of
	[{CallID,CallID}|_] ->
	    case msgpack:unpack(MPRC#mprc.carry) of
		{[?MP_TYPE_RESPONSE, CallID, Error, Retval], RemainBin}->
		    MPRC0 = MPRC#mprc{carry=RemainBin},
		    msgpack_util:call_done(CallID),
		    case Error of
			nil ->
			    %?debugVal(Retval),
			    %?debugVal(Remain),
			    %?debugVal(ets:tab2list(?MODULE)),
			    join_(MPRC0, Remain, [Retval|Got]);
			_Other -> 
			    join_(MPRC0, Remain, [{error, {Error,Retval}}|Got])
		    end;
		{[?MP_TYPE_RESPONSE, CallID0, Error, Retval], RemainBin}->
		    msgpack_util:insert({CallID0, Error, Retval}),
		    MPRC0 = MPRC#mprc{carry=RemainBin},
		    join_(MPRC0, [CallID|Remain], Got);
		{error, incomplete} ->
		    {ok, PackedMsg}  = gen_tcp:recv(MPRC#mprc.s, 0),
		    NewBin = <<(MPRC#mprc.carry)/binary, PackedMsg/binary>>,
		    join_(MPRC#mprc{carry=NewBin}, Remain, Got);
		{error, Reason} ->
		    {error, Reason}
	    end;
	[{CallID,Error,Retval}|_] ->
	    msgpack_util:call_done(CallID),
	    case Error of
		nil ->
		    join_(MPRC, Remain, [Retval|Got]);
		_Other -> 
		    join_(MPRC, Remain, [{error, {Error,Retval}}|Got])
	    end
	end;
join_(MPRC, Remain, Got) ->
    %?debugVal(Remain), %?debugVal(MPRC),
    {ok, PackedMsg}  = gen_tcp:recv(MPRC#mprc.s, 0),
    NewBin = <<(MPRC#mprc.carry)/binary, PackedMsg/binary>>,
    join_(MPRC#mprc{carry=NewBin}, Remain, Got).

-spec close(mprc()) -> ok|{error,term()}.		    
close(Client)-> gen_tcp:close(Client#mprc.s).

-spec controlling_process(mprc())-> ok.
controlling_process(MPRC)->				 
    gen_tcp:controlling_process(MPRC#mprc.s, self()).

-spec active_once(mprc())-> ok.
active_once(MPRC)->
    inet:setopts(MPRC#mprc.s, [{active,once}]).

append_binary(MPRC, Bin)->
    NewBin = <<(MPRC#mprc.carry)/binary, Bin/binary>>,
    MPRC#mprc{carry=NewBin}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_options(_)->
    ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_test_()->
    {setup, local,
     fun()-> mprc:start() end,
     fun(_)-> mprc:stop() end,
     [
     ]
    }.

-endif.
