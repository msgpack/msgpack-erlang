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

-record(options, 
	{ transport = tcp :: transport(),
	  ip = inet:: inet | inet6,
	  host :: inet:ip_address(),
	  port :: nport() }).

%% external API
-export([start/0, stop/0,
	 connect/3, close/1, call/3, call_async/3, join/2, notify/3,
	 controlling_process/2, active_once/1, append_binary/2]).

%%====================================================================
%% API
%%====================================================================
-spec start()-> ok.
start()->
    msgpack_util:start().

-spec stop()-> ok.
stop()->
    msgpack_util:stop().

-spec connect(gen_tcp:ip_address(), inet:port_number(), Options::[term()])->
		     {ok, mprc()}|{error,Reason::term()}.
connect(Address, Port, Options)->
    Opts = parse_options(Options, #options{}),
    TransportModule = get_module(Opts),
    TransportModule:connect(Address, Port, make_options(Opts)).

% @doc synchronous calls
% when method 'Method' doesn't exist in server implementation,
% it returns {error, {<<"no such method">>, nil}}
% user func error => {error, {<<"unexpected error">>, nil}}
% @end
-spec call(mprc(), Method::atom(), Argv::list()) -> {term(), mprc()} | {error, {atom(), any()}}.
call(#mprc{transport=tcp}=MPRC, Method, Argv) when is_atom(Method), is_list(Argv) ->
    mprc_tcp:call(MPRC, Method, Argv);
call(#mprc{transport=udp}=MPRC, Method, Argv) when is_atom(Method), is_list(Argv) ->
    mprc_udp:call(MPRC, Method, Argv).

-spec call_async(mprc(), atom(), [term()]) -> {ok, non_neg_integer()}.
call_async(#mprc{transport=tcp}=MPRC,Method,Argv)->
    mprc_tcp:call_async(MPRC, Method, Argv);
call_async(#mprc{transport=udp}=MPRC,Method,Argv)->
    mprc_udp:call_async(MPRC, Method, Argv).

-spec join(mprc(), integer() | [integer()]) -> {term(), mprc} | {[term()], mprc()} | {error, term()}.
join(#mprc{transport=tcp}=MPRC, CallIDs)->
    mprc_tcp:join(MPRC, CallIDs);
join(#mprc{transport=udp}=MPRC, CallIDs)->
    mprc_udp:join(MPRC, CallIDs).

-spec notify(mprc(), atom(), [term()])-> ok | {error, term()}.
notify(#mprc{transport=tcp}=MPRC, Method, Argv)->
    mprc_tcp:notify(MPRC,Method,Argv);
notify(#mprc{transport=udp}=MPRC, Method, Argv)->
    mprc_udp:notify(MPRC,Method,Argv).

-spec close(mprc()) -> ok.
close(#mprc{transport=tcp}=MPRC)-> mprc_tcp:close(MPRC);
close(#mprc{transport=udp}=MPRC)-> mprc_udp:close(MPRC).

-spec controlling_process(mprc(), pid())-> ok.
controlling_process(#mprc{transport=tcp}=MPRC, Pid)->
    mprc_tcp:controlling_process(MPRC,Pid).

-spec active_once(mprc())-> ok.
active_once(#mprc{transport=tcp}=MPRC)->
    mprc_tcp:active_once(MPRC).

append_binary(#mprc{transport=tcp}=MPRC, Bin)->
    mprc_tcp:append_binary(MPRC, Bin).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec parse_options([ atom() | {atom(), term()}], #options{}) -> #options{}.
parse_options([], Options)-> Options;
parse_options([tcp|L], Options) ->
    parse_options(L, Options#options{transport=tcp});
parse_options([udp|L], Options) ->
    parse_options(L, Options#options{transport=udp});
parse_options([inet|L], Options) ->
    parse_options(L, Options#options{ip=inet});
parse_options([{host,Host}|L],Options)->
    parse_options(L, Options#options{host=Host});
parse_options([{port,Port}|L], Options) when is_integer(Port), 0 < Port , Port < 65536->
    parse_options(L, Options#options{port=Port});
parse_options([Opt|_], _)->
    {error, {not_supported, Opt}}.
%% parse_options([sctp|L], Options) ->
%%     {error, {not_supported, sctp}};
%% parse_options([uds|L], Options) ->
%%     {error, {not_supported, uds}};

-spec get_module(#options{}) -> mprc_tcp | mprc_udp.
get_module(#options{transport=tcp}=_Opts)-> mprc_tcp;
get_module(#options{transport=udp}=_Opts)-> mprc_udp.

-spec make_options(#options{}) -> [gen_tcp:connect_option()].
make_options(#options{transport=udp}=Opt)->
    [Opt#options.ip, binary, {active, false}];
make_options(Opt)->
%    [{host,Opt#options.host}, {port,Opt#options.port},
    [Opt#options.ip, {packet,0}, binary, {active, false}].


