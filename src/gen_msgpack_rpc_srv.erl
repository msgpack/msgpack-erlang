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

-module(gen_msgpack_rpc_srv).

-behaviour(gen_server).
-include("msgpack_rpc.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2, notify/3, behaviour_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,1}, {handle_call,3}, {terminate,2}, {code_change,3}];
behaviour_info(_Other) ->
    undefined.

-record(state, {socket :: inet:socket(),
		module :: atom(),
		context :: term(),
		carry = <<>> :: binary()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Description: [Module]++[Socket] will come. 
-spec start_link(atom(), inet:socket()) -> {ok,pid()}.
start_link(Module,Socket) when is_atom(Module), is_port(Socket)->
%    gen_server:start_link(?MODULE, [Module,Socket], [{debug,[trace,log,statistics]}]).
    {ok,_Pid}=gen_server:start_link(?MODULE, [Module,Socket], []).

% TODO/TBF
-spec notify(term(), atom(), [term()]) -> ok.
notify(Id, Method, Argv) when is_atom(Method) andalso is_list(Argv) ->
    Meth = atom_to_binary(Method,latin1),
    gen_server:cast(Id, {notify, Meth, Argv}). % don't know why it doesn't work => goes {error, einval}

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    io:format("~p~p: ~p~n", [?FILE, ?LINE, ?MODULE]),
    {stop, {error,badarg}};
init([Module,Socket]) when is_atom(Module), is_port(Socket)->
    %[active, nodelay, keepalive, delay_send, priority, tos]) of

    case Module:init([]) of
	{ok, Context}->
	    ok=inet:setopts(Socket, [{active,once},{packet,raw},binary]),
	    {ok, #state{module=Module, socket=Socket, context=Context}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, From, #state{context=Context,module=Module}=State) ->
    case Module:handle_call(Request, From, Context) of
	{reply, Reply, NextContext}->
	    {reply, Reply, State#state{context=NextContext}};
	{noreply, NextContext}->
	    {noreply, State#state{context=NextContext}};
	{stop, Reason, Reply, NextContext} ->
	    {stop, Reason, Reply, State#state{context=NextContext}}
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({notify, Method, Argv}, #state{socket=Socket}=State) ->
    %% Pid = self(),
    %% ok=gen_tcp:controlling_process(Socket,Pid),
    %% inet:setopts(Socket,[binary,raw,{active,false}]),
    case msgpack:pack([?MP_TYPE_NOTIFY, Method, Argv]) of
	Msg when is_binary(Msg) ->
	    gen_tcp:send(Socket, Msg);
	_Error ->
	    error_logger:info_msg("~s:~p ~p .\n", [?FILE, ?LINE,_Error])
    end,
    {noreply, State};

handle_cast(Msg, State)->
    error_logger:info_msg("~s:~p unknown message: ~p .\n", [?FILE, ?LINE, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, #state{socket=Socket,carry=Carry} = State) ->
    NewBin = <<Carry/binary, Bin/binary>>,
    process_binary(State#state{carry=NewBin});

handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) ->
%    error_logger:debug_msg("~p Client ~p disconnected.\n", [self(), hoge]),
    {stop, normal, State};

handle_info({do_reply, ReplyBin}, #state{socket=Socket} = State) ->
    ok = gen_tcp:send(Socket, ReplyBin),
    {noreply, State};

handle_info(_Info, State) ->
    error_logger:info_msg("~p: unknown message: ~p .\n", [self(), _Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{module=Module, context=Context, socket=Socket}) ->
    Module:terminate(Reason,Context),
    ok=gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, #state{module=Module, context=Context} = State, Extra) ->
    {ok, NextState}=Module:code_change(OldVsn, Context, Extra),
    {ok, State#state{context=NextState}}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec process_binary(#state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
process_binary(#state{socket=Socket, module=Module, context=Context, carry=Bin} = State)->
    case msgpack:unpack(Bin) of
	{[?MP_TYPE_NOTIFY,M,Argv], Remain} ->
	    handle_notify(Module, M, Argv),
	    process_binary(State#state{context=Context, carry=Remain});
	{[?MP_TYPE_REQUEST,CallID,M,Argv], Remain} ->
	    spawn_request_handler(CallID, Module, M, Argv),
	    process_binary(State#state{context=Context, carry=Remain});
	{error, incomplete} -> % carry over
	    ok=inet:setopts(Socket, [{active,once}]),
	    {noreply, State};
	{error, Reason} ->
	    error_logger:error_msg("error: ~p~n", [Reason]),
	    {stop, Reason, State}
    end.

-spec handle_notify(atom(), binary(), [term()]) -> pid().
handle_notify(Module, M, Argv)->
    spawn(fun()->
		  Method = binary_to_existing_atom(M, latin1),
		  erlang:apply(Module, Method, Argv)
	  end).

-spec spawn_request_handler(integer(), atom(), binary(), [term()]) -> pid().
spawn_request_handler(CallID, Module, M, Argv)->
    Self = self(),
    F= fun()->
	       Method = binary_to_existing_atom(M, latin1),
	       Prefix = [?MP_TYPE_RESPONSE, CallID],
	       Ret = case erlang:apply(Module,Method,Argv) of
			 {reply, true} ->   [nil, true];
			 {reply, false} ->  [nil, false];
			 {reply, Result} when is_atom(Result) ->  [nil, atom_to_binary(Result,latin1)];
			 {reply, Result} -> [nil, Result];
			 {error, Reason} when is_atom(Reason)->   [Reason, nil];
			 {error, Reason} -> [Reason, nil]
		     end,
	       Self ! {do_reply, msgpack:pack(Prefix++Ret)}
       end,
    spawn_link(F).
