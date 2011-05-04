%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2010 UENISHI Kota
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

-module(gen_msgpack_rpc_test).

-behaviour(gen_msgpack_rpc).

-export([init/1, handle_call/3, terminate/2, code_change/3]).

-record(state, {}).

init(_)->
    {ok, #state{}}.

handle_call(_Msg,_From,State)->
    {noreply, State}.

terminate(_Reason,_State)->
    ok.

code_change(_,State,_)->
    {ok,State}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

easy_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok=mprc:start(),

    {ok, Pid2}=gen_msgpack_rpc:start_link({local,?MODULE},?MODULE,localhost,9199,[tcp]),
    
    ?assertEqual({ok,3}, gen_msgpack_rpc:call(Pid2, add, [1, 2])),
    ?assertEqual({ok,3}, gen_msgpack_rpc:call(?MODULE, add, [1, 2])),

    {ok, CallID} = gen_msgpack_rpc:call_async(Pid2, add, [1,2]),
    receive
	{CallID, {ok,3}} -> ok;
	_ -> ?assert(false)
    end,
    
    ok=gen_msgpack_rpc:stop(Pid2),

    ok=mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.


loop_receive(0, List)-> lists:keysort(1,List);
loop_receive(N, List)->
    receive
	{CID, {ok,S}} ->
	    loop_receive(N-1, [{CID,S}|List]);
	_R -> ?debugVal(_R), ?assert(false), List
    end.

easy2_test()->
    {ok,Pid} = mprs_tcp:start_link(sample_srv, [{host,localhost},{port,9199}]),
    ?assert(is_pid(Pid)),

    ok=mprc:start(),

    {ok, Pid2}=gen_msgpack_rpc:start_link({local,?MODULE},?MODULE,localhost,9199,[tcp]),
    
    Pairs=[{5,5}, {0,0}, {234, 2}, {213456789, -3}, {234, -23}, {-1,1}, {1,-1}, {-1,-1},
	   {-2000, 2000}, {2000, -2000}, {234, -234}],
    lists:map( fun({L,R})->
		       ?assertEqual({ok,L+R}, gen_msgpack_rpc:call(Pid2, add, [L,R]))
	       end, Pairs ),

    CallIDs = lists:map( fun({L,R})->
				 {ok,CallID}=gen_msgpack_rpc:call_async(Pid2, add, [L,R]),
				 {CallID, L+R}
			 end, Pairs ),
    Results = loop_receive(length(CallIDs), []),
    ?assertEqual(lists:keysort(1,CallIDs), Results),

    ?assertEqual({error, <<"no such func">>}, gen_msgpack_rpc:call(Pid2, addhage, [])),

    ok=gen_msgpack_rpc:stop(Pid2),
    ok=mprc:stop(),

    ?assertEqual(ok,gen_server:call(Pid,stop)),
    ok.

-endif.
