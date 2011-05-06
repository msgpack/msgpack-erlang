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
-module(mprs_udp).

-behaviour(gen_server).
-include("msgpack_rpc.hrl").

%% API
-export([start_link/2, start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([dispatch/5]).

-define(SERVER, ?MODULE).

-record(state, {socket :: inet:socket(),
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
    Opts = [binary, {reuseaddr, true}, {active, false}, {ip, Addr}],
%    ?debugVal(Opts),
    case gen_udp:open(Port,Opts) of % FIXME: inet6
	{ok, Socket} ->
	    %%Create first accepting process
	    inet:setopts(Socket,[{active,once}]),
	    {ok, #state{socket = Socket, module = Mod}};
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
handle_info({udp, Socket, IP, InPortNo, Packet}, #state{socket=Socket} = State) ->
    spawn(?MODULE, dispatch, [self(),IP, InPortNo, Packet, State]),
%    erlang:apply(fun ?MODULE:dispatch/5, [self(),IP, InPortNo, Packet, State]),
    %% Signal the network driver that we are ready to accept another connection
    inet:setopts(Socket, [{active,once}]),
    {noreply, State};

handle_info({send, Socket, IP, InPortNo, Packet}, State) ->
    ok=gen_udp:send(Socket, IP, InPortNo, Packet),
    {noreply, State};

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
    gen_udp:close(State#state.socket).

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
dispatch(From, IP, InPortNo, Packet, #state{module=Module,socket=Socket}=State)->
    case msgpack:unpack(Packet) of
	{error, incomplete}->
	    error_logger:error_report([?MODULE, ?LINE, {IP,InPortNo,State, Packet}]);
	{[?MP_TYPE_REQUEST,CallID,M,Argv], Remain}->
	    Method = binary_to_atom(M, latin1),
	    ReplyBin = 
		try
		    case erlang:apply(Module,Method,Argv) of
			{reply, Result} ->
			    msgpack:pack([?MP_TYPE_RESPONSE, CallID, nil, Result]);
			{error, Reason}->
			    msgpack:pack([?MP_TYPE_RESPONSE, CallID, Reason, nil])
		    end
		catch
		    _:undef ->
			error_logger:error_msg("~s:~p no such method: ~p:~s/~p~n",
					       [?FILE,?LINE,Module,binary_to_list(M),length(Argv)]),
			Msg = << (<<"no such method: ">>)/binary, M/binary>>,
			msgpack:pack([?MP_TYPE_RESPONSE, CallID, Msg, nil]);
		    _:What ->
			Msg = [?MP_TYPE_RESPONSE, CallID, <<"unexpected error">>, nil],
			error_logger:error_msg("unknown error: ~p (~p:~s/~p)~n",
					       [What, Module,binary_to_list(M),length(Argv)]),
			msgpack:pack([?MP_TYPE_RESPONSE, CallID, Msg, nil])
		end,
	    From ! {send, Socket, IP, InPortNo, ReplyBin},
	    case Remain of
		<<>> -> ok;
		_ ->    dispatch(From, IP, InPortNo, Remain, State)
	    end
    end.
