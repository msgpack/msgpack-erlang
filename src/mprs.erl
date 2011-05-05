%%%-------------------------------------------------------------------
%%% File    : mprs.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  5 May 2011 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mprs).

-export([start_link/2, start_link/3,
	 stop/0, stop/1]).
-include("msgpack_rpc.hrl").

-record(options, 
	{ transport = tcp :: transport(),
	  ip = inet:: inet | inet6,
	  host :: inet:ip_address(),
	  port :: nport() }).

-spec start_link(atom(), #options{})-> {ok, pid()}.
start_link(Module, Options)->
    start_link({local, ?MODULE}, Module, Options).

-spec start_link(term(), atom(), #options{})-> {ok, pid()}.
start_link(Name,Module,Options)->
    Opts = parse_options(Options, #options{}),
    TransportMod = get_module(Opts),
    TransportMod:start_link(Name, Module, make_options(Opts)).

-spec stop()-> ok.
stop()->
    gen_server:call(?MODULE, stop).

-spec stop(term())-> ok.
stop(Pid)->
    gen_server:call(Pid, stop).

parse_options([], Options)-> Options;
parse_options([tcp|L], Options) ->
    parse_options(L, Options#options{transport=tcp});
parse_options([inet|L], Options) ->
    parse_options(L, Options#options{ip=inet});
parse_options([{host,Host}|L],Options)->
    parse_options(L, Options#options{host=Host});
parse_options([{port,Port}|L], Options) when is_integer(Port), 0 < Port , Port < 65536->
    parse_options(L, Options#options{port=Port});
parse_options([Opt|_], _)->
    {error, {not_supported, Opt}}.
%% parse_options([udp|L], Options) ->
%%     {error, {not_supported, udp}};
%% parse_options([sctp|L], Options) ->
%%     {error, {not_supported, sctp}};
%% parse_options([uds|L], Options) ->
%%     {error, {not_supported, uds}};
%% parse_options([

get_module(#options{transport=tcp}=_Opts)-> mprs_tcp.

make_options(Opt)->
    [{host,Opt#options.host}, {port,Opt#options.port},
     Opt#options.ip, {packet,raw}, binary, {active, false}].
