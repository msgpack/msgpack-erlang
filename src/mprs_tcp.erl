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
%%%
%%% @end
-module(mprs_tcp).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listener :: inet:socket(),
		acceptor :: erlang:ref(),
		module :: atom()}).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% TODO: maket start_link/3 for multiple listening
%% @end
%%--------------------------------------------------------------------
-spec start_link(Mod::atom(), [term()]) ->
			{ok, Pid::pid()} | ignore | {error, Error::term()}.
start_link(Mod, Options) ->
    start_link({local, ?SERVER}, Mod, Options).

-spec start_link(Id::atom(), Mod::atom(), [term()]) ->
			{ok, Pid::pid()} | ignore | {error, Error::term()}.
start_link(Id, Mod, Options) ->
    gen_server:start_link(Id, ?MODULE, [Mod, Options], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([Mod,Options]) ->
    {Host,Options0} = msgpack_util:pppop(host,Options),
    {Port,_Options1} = msgpack_util:pppop(port,Options0),
    init_(Mod,Host,Port).

init_(Mod, Host, Port) when is_atom(Host)->
    {ok,Addr} = inet:getaddr(Host, inet),
    init_(Mod, Addr, Port);
init_(Mod, Host, Port) when is_list(Host)->
    {ok,Addr} = inet:getaddr(Host, inet),
    init_(Mod, Addr, Port);
init_(Mod, Addr, Port) when is_atom(Mod), 0<Port, Port<65535, is_tuple(Addr)->
    Opts = [binary, {packet, raw}, {reuseaddr, true},
            {keepalive, true}, {backlog, 30}, {active, false}, {ip, Addr}
	   ],
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, Opts) of
	{ok, ListenSocket} ->
	    %%Create first accepting process
	    {ok, Ref} = prim_inet:async_accept(ListenSocket, -1),
	    {ok, #state{listener = ListenSocket,
			acceptor = Ref,
			module = Mod}};
	{error, Reason} ->
	    ?debugVal(Reason),
	    {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _, State)->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, {ok, abnormal}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, acceptor=Ref, module=Mod} = State) ->
    try
%	inet:setsockopts(ListSock, [{active,once}]),
	ok=set_sockopt(ListSock, CliSocket),

        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
	{ok,Pid}=gen_msgpack_rpc_srv:start_link(Mod,CliSocket),
%	{ok,Pid}=proc_lib:start_link(Mod, start_link, [CliSocket]),
        ok=gen_tcp:controlling_process(CliSocket, Pid),

        %% Signal the network driver that we are ready to accept another connection
        case prim_inet:async_accept(ListSock, -1) of
	    {ok,    NewRef} -> ok;
	    {error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
        end,
        {noreply, State#state{acceptor=NewRef}}

    catch exit:Why ->
        error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
        {stop, Why, State}
    end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info({'EXIT', _Pid, normal}, State) ->
%    error_logger:error_report({?MODULE, ?LINE, {State}}),
    {noreply, State};
handle_info({'EXIT', _Pid, Reason}, State) ->
    %% If there was an unexpected error accepting, log and sleep.
    error_logger:error_report({?MODULE, ?LINE, {error, Reason}}),
    {noreply, State};

handle_info(_Info, State) ->
    error_logger:error_report({?MODULE, ?LINE, {_Info}}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
