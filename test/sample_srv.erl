%%%-------------------------------------------------------------------
%%% File    : sample_srv.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%% @hidden
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_srv).
-author('kuenishi@gmail.com').

-behaviour(gen_msgpack_rpc_srv).

-include_lib("eunit/include/eunit.hrl").

%% rpc methods
-export([hello/0, add/2]).

%% API
-export([init/1, handle_call/3, terminate/2, code_change/3]).

-record(state, {}).
%%====================================================================
%% API
%%====================================================================
init(_Argv)->
    {ok, #state{}}.

hello()->
    {reply, "hello, msgpack!"}.

add(I, J) when is_integer(I) andalso is_integer(J)->
    {reply, I+J}.

handle_call(_Request, _From, State)->
    Reply=ok,
    {reply, Reply, State}.

terminate(_Reason, State)->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================


easy_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok = mprc:start(),
    {ok,S} = mprc:connect(localhost,9199,[]),
    ?assert(is_port(S)),

    ?assertEqual("hello, msgpack!", mprc:call(S, hello, [])),
    ?assertEqual(234, mprc:call(S, add, [230,4])),
    A=2937845, B=238945-29038453490, C=A+B,
    ?assertEqual(C, mprc:call(S, add, [A,B])),
% TODO: make it exception thrown
%    ?assertEqual(234, mprc:call(S, addo, [230,0])),
    
    ?assertEqual(ok, mprc:close(S)),
    ok = mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.
