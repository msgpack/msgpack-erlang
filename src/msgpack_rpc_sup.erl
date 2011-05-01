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

-module(msgpack_rpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/2, start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type option() :: {host, inet:ip_address()|inet:hostname()}|{port, inet:ip_port()}|
		  {transport, msgpack_rpc:transport()}.

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), [option()])-> {ok, pid()}.
start_link(Mod,Options) ->
    msgpack_rpc_sup:start_link(?SERVER, ?MODULE, [Mod,Options]).

-spec start_link(Id::atom(), atom(), [option()])-> {ok, pid()}.
start_link(Id,Mod,Options) ->
    supervisor:start_link({loacl,Id}, ?MODULE, [Id,Mod,Options]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Id,Mod,Options]) ->
    % Options for tcp ports
    {Host, Options0} = msgpack_util:pppop(host, Options),
    {Port, Options1} = msgpack_util:pppop(port, Options0),
    % remain goes to user defined modules

    {Transport, Options2} = msgpack_util:pppop(transport, Options1),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs = children(Transport, Host, Port, Id, Mod, Options2),
    {ok, {SupFlags, ChildSpecs}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

children(tcp,  Host, Port, Id, Mod, Options)->

    Restart = permanent,
    Shutdown = 2000,

    Children = [{msgpack_rpc_sessions,
		 { msgpack_rpc_sessions,start_link,[Mod,Options]},
		 Restart, Shutdown, supervisor, []},
                {msgpack_util:append_srv(Id),
		 {msgpack_rpc_listener_tcp,start_link,[Host,Port]},
		 Restart, Shutdown, worker, []}],
    ok=supervisor:check_childspecs(Children),
    Children;

children(udp, Host, Port, _Id, Mod, Options) ->
    Restart = permanent,
    Shutdown = 2000,

    Children = [{msgpack_rpc_listener_udp,
		 {msgpack_rpc_listener_udp,start_link,[Host,Port,Mod, Options]},
		 Restart, Shutdown, worker, []}],
    ok=supervisor:check_childspecs(Children),
    Children;

children(T, _, _, _, _, _) ->
    {error, {not_supported,T}}.
