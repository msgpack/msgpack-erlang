%%%-------------------------------------------------------------------
%%% File    : sample_srv.erl
%%% Description : 
%%% @hidden
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample_srv).

-include_lib("eunit/include/eunit.hrl").

%% rpc methods
-export([hello/0, add/2, send_notify/2]).

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
%    ?debugVal({I,J}),
    {reply, I+J}.

send_notify(Num,BinPid) when is_integer(Num) andalso Num > 0 andalso is_binary(BinPid) ->
    ?debugVal(binary_to_term(BinPid)),
    gen_msgpack_rpc_srv:notify(get_notify, [BinPid]),
    {reply, ok}.

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
