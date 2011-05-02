%%%-------------------------------------------------------------------
%%% File    : sample_client.erl
%%% Description : 
%%% @hidden
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_client).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_msgpack_rpc).
-compile(export_all).

-record(state, {}).

init(_Argv)->
    {ok, #state{}}.

hello()->
    {"hello, msgpack!"}.

sub(I, J) when is_integer(I) andalso is_integer(J)->
						%?debugVal({I,J}),
    {I+J}.

handle_call(_Request, _From, State)->
    Reply=ok,
    {reply, Reply, State}.

terminate(_Reason, State)->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% loop(_,  0) -> ok;
%% loop(Fun,N) ->
%%     Fun(N),
%%     loop(Fun,N-1).

%% load(P)->
%%     ok = sample_app:start(),
%%     {ok, _Pid} = mp_client:connect(localhost,65500),
%%     ReqFun = fun(N)-> {ok, _} = mp_client:call(N, hello, []) end,
%%     loop(ReqFun, P),
%%     ok = mp_client:close(),
%%     ok = sample_app:stop().

easy_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),
    
    

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.

