%%%-------------------------------------------------------------------
%%% File    : mprs_test.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  5 May 2011 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mprs_test).

-include_lib("eunit/include/eunit.hrl").

easy_test()->
    {ok,Pid}=mprs:start_link(sample_srv, [{host,localhost},{port,9199}]),

    ok=mprc:start(),
    {ok, Pid2}=gen_msgpack_rpc:start_link({local,?MODULE},?MODULE,localhost,9199,[tcp]),
    
    ?assertEqual({ok,<<"hello, msgpack!">>}, gen_msgpack_rpc:call(Pid2, hello, [])),
    ?assertEqual({ok,3}, gen_msgpack_rpc:call(Pid2, add, [1, 2])),
    ?assertEqual({ok,125}, gen_msgpack_rpc:call(?MODULE, add, [123, 2])),

    ok=gen_msgpack_rpc:stop(Pid2),
    ok=mprc:stop(),

    ?assertEqual(ok,mprs:stop(Pid)).
