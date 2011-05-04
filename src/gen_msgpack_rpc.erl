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
%%   gen_msgpack_rpc is a client interface to access messagepack server.
%%   in erlang way, this is just a OTP worker. You can set your
%%   code into your OTP supervision tree like a gen_server.
%%
%%   current status
%%     only TCP works, now rewriting as to work also UDP works.
%%
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

-module(gen_msgpack_rpc).

-behaviour(gen_server).
-include("msgpack_rpc.hrl").

-define(SERVER, ?MODULE).

%% external API
-export([start_link/5, stop/1, behaviour_info/1]).

-export([call/3,
	 call_async/3, call_async/4, cancel_async_call/1, cancel_async_call/2
	]).

%% internal: gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-type address() :: string()|atom()|inet:ip_address().

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1}, {handle_call,3}, {terminate,2}, {code_change,3}];
behaviour_info(_Other) ->
    undefined.

-record(state, { module :: atom(), mprc :: mprc:mprc() }).

%%====================================================================
%% API
%%====================================================================

-spec start_link(Identifier::term(), Module::atom(),
		 Address::address(), Port::(0..65535), [term()]) ->  {ok, pid()}.
start_link(I,M,A,P,Options) -> 
    {ok,MPRC}=mprc:connect(A,P,Options), % needs {active, false}
    case gen_server:start_link(I, ?MODULE, [M,MPRC,Options], []) of
% for debug			  [{debug,[trace,log,statistics]}]).
	{ok, Pid} ->
	    ok=mprc:controlling_process(MPRC, Pid),
	    {ok, Pid};
	{error, Reason} ->
	    mprc:close(MPRC),
	    {error, Reason}
    end.

-spec stop(Identifier::term()) -> ok.
stop(Id)->
    gen_server:call(Id, stop).

% synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
-spec call(Id::term(), Method::atom(), Argv::list()) -> 
    {ok, any()} | {error, {atom(), any()}}.
call(Id, Method, Argv) ->
    gen_server:call(Id, {call_async, Method, Argv}, 0).

% TODO: write test code for call_async/3
% afterwards, the caller will receive the response {ok, ResCode, Result} as a message
-spec call_async(CallID::non_neg_integer(),
		 Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
call_async(CallID, Method, Argv)->
    call_async(?SERVER, CallID, Method, Argv).

-spec call_async(Client::server_ref(), CallID::non_neg_integer(),
		 Method::atom(), Argv::list()) -> ok | {error, {atom(), any()}}.
call_async(Client, CallID, Method, Argv) when is_atom(Method), is_list(Argv)->
    Meth = <<(atom_to_binary(Method,latin1))/binary>>,
    Pid = self(),
    case msgpack:pack([?MP_TYPE_REQUEST,CallID,Meth,Argv]) of
	{error, Reason}->
	    {error, Reason};
	Pack ->
	    ok=gen_server:call(Client, {call, {CallID, Pid} ,Pack})
    end.

% TODO: write test code for cancellation
-spec cancel_async_call(CallID::non_neg_integer())->ok.
cancel_async_call(CallID)-> cancel_async_call(?SERVER, CallID).

-spec cancel_async_call(Client::server_ref(), CallID::non_neg_integer())->ok.
cancel_async_call(Client, CallID)->
    gen_server:call(Client, {cancel, CallID}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
%% Function: init(Args) -> {ok, State} |%
%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Mod, MPRC, _Options])->
    {ok, #state{module=Mod, mprc = MPRC}}.

%% @private
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({call, Method, Argv}, From, #state{mprc=MPRC} = State)->
    CallID = mprc:call_async(MPRC, Method, Argv),
    msgpack_util:insert({CallID,From}),
    {reply, {ok, CallID}, State};

handle_call(stop, _From, State)->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Pack}, #state{module=Mod,mprc=MPRC}=State)->
    case msgpack:unpack(Pack) of
	{[?MP_TYPE_NOTIFICATION,Method,Params],_RemBin}->
	    % maybe we need string whilelist for atom-attack
	    Meth = binary_to_atom(Method, latin1),
	    try
		_Ret=erlang:apply(Mod,Meth,Params)
	    catch _:_Other ->
		    error_logger:error_msg("error ~s~p: ~p~n", [?FILE, ?LINE, _Other])
	    end,

	    {noreply, State};
	{[?MP_TYPE_RESPONSE,CallID,ResCode,Result],RemBin} ->
	    case msgpack_util:lookup(CallID) of
		[]->
		    ok = mprc:active_once(MPRC),
		    {noreply, State#state{mprc=mprc:append_binary(MPRC,RemBin)}};
		[{CallID,From}|_] ->
		    msgpack_util:call_done(CallID),
		    case ResCode of
			nil -> gen_server:reply(From, Result);
			Error -> gen_server:reply(From, {Error,Result})
		    end,
		    ok = mprc:active_once(MPRC),
		    {noreply, State#state{mprc=mprc:append_binary(MPRC,RemBin)}};
		_Other ->		% Error
		    io:format("error ~s~p: ~p~n", [?FILE, ?LINE, _Other]),
		    ok = mprc:active_once(MPRC),
		    {noreply, State}
	    end;
	{error, incomplete} ->
	    ok = mprc:active_once(MPRC),
	    {noreply, State#state{mprc=mprc:append_binary(MPRC,Pack)}};
	{error, _Reason} ->
	    ok = mprc:active_once(MPRC),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    mprc:close(State#state.mprc),
    ok.

%% @private
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-ifdef(TEST).
% tests moved to test/gen_msgpack_rpc_test.erl
-endif.
