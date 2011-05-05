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
    application:start(sample_app).

stop()->
    application:stop(sample_app).

