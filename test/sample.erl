%%%-------------------------------------------------------------------
%%% File    : sample.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  5 May 2011 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(sample).

-export([start/0, stop/0]).

start()->
    ok=application:start(sample).

stop()->
    ok=application:stop(sample),
    halt(). % FIXME: separate from stop/0 call.

-include_lib("eunit/include/eunit.hrl").

%% easy_test()->
%%     ?debugVal(c:pwd()),
%%     {ok,Pid}=sample:start(),
%%     ?assert(is_pid(Pid)).
%    erlang:exit(Pid,normal).
