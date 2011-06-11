%%%-------------------------------------------------------------------
%%% File    : sample_srv.erl
%%% Description : 
%%% @hidden
%%% Created :  5 Jun 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(mprc_test).

-include_lib("eunit/include/eunit.hrl").

easy_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok = mprc:start(),
    {ok,S} = mprc:connect(localhost,9199,[]),

    {Ret,MPRC0} = mprc:call(S, hello, []), 
    ?assertEqual(Ret, {ok,<<"hello, msgpack!">>}),
    {Ret0, MPRC1} = mprc:call(MPRC0, add, [230,4]),
    ?assertEqual({ok,234}, Ret0),
    A=2937845, B=238945029038453490, C=A+B,
    {Ret1, MPRC2} = mprc:call(MPRC1, add, [A,B]),
    ok = mprc:notify(MPRC2, add, [A,B]),
    ?assertEqual({ok,C}, Ret1),
% TODO: make it exception thrown
%    ?assertEqual(234, mprc:call(S, addo, [230,0])),
    
    {ok, CallID0} = mprc:call_async(MPRC2, add, [23, -23]),
    {ok, CallID1} = mprc:call_async(MPRC2, add, [23, 23]),
    {Ans, _MPRC3} = mprc:join(MPRC2, [CallID0, CallID1]),
    ?assertEqual([{ok,46},{ok,0}], Ans),
%    %?debugVal(mprc:join(_MPRC3, CallID2)),
    
    ?assertEqual(ok, mprc:close(S)),
    ok = mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.

easy2_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok = mprc:start(),
    {ok,S} = mprc:connect(localhost,9199,[]),

    {ok, CallID0} = mprc:call_async(S, add, [2, -23]),
    {ok, CallID1} = mprc:call_async(S, add, [2, 23]),
    {ok, CallID2} = mprc:call_async(S, add, [2, 1]),
    {ok, CallID3} = mprc:call_async(S, add, [2, 1]),
    {Ans, _MPRC3} = mprc:join(S, [CallID0, CallID1, CallID2,CallID3]),
    Ans0 = lists:sort(lists:map(fun({ok,R})->R end, Ans)),
    ?assertEqual(lists:sort([-21,3,3,25]), Ans0),
    
    ?assertEqual(ok, mprc:close(S)),
    ok = mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.


conn_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok = mprc:start(),
    Arr = lists:seq(1,200), % we need more
    MPRCs = lists:map(fun(_)-> {ok,S} = mprc:connect(localhost,9199,[]), S end, Arr),

    lists:map(fun(MPRC)->
		      {Ret,_MPRC0} = mprc:call(MPRC, add, [234, -34]),
		      ?assertEqual({ok,200}, Ret)
	      end, MPRCs),

    lists:map(fun(MPRC)-> ?assertEqual(ok, mprc:close(MPRC)) end, MPRCs),
    ok = mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.
    

% UDP test
easy3_test()->
    {ok,Pid} = mprs:start_link(sample_srv, [{host,localhost},{port,9199},udp]),
    ?assert(is_pid(Pid)),

    ok = mprc:start(),
    {ok,S} = mprc:connect(localhost,9199,[udp]),

    {Ret, _} = mprc:call(S, hello, []), 
    ?assertEqual(Ret, {ok,<<"hello, msgpack!">>}),
    {Ret0, _} = mprc:call(S, add, [230,4]),
    ?assertEqual({ok,234}, Ret0),
    A=2937845, B=238945029038453490, C=A+B,
    {Ret1, _} = mprc:call(S, add, [A,B]),
    ?assertEqual({ok,C}, Ret1),
%% % TODO: make it exception thrown
%% %    ?assertEqual(234, mprc:call(S, addo, [230,0])),
    {ok, CallID0} = mprc:call_async(S, add, [23, -23]),
    {ok, CallID1} = mprc:call_async(S, add, [23, 23]),
    {Ans, _MPRC3} = mprc:join(S, [CallID0, CallID1]),
    ?assertEqual(Ans, [{ok,46},{ok,0}]), % maybe reversed
%    ?debugVal(mprc:join(_MPRC3, CallID2)),
    
    ?assertEqual(ok, mprc:close(S)),
    ok = mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.
    
