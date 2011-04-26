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

%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2010, UENISHI Kota
%%% @doc
%%   mp_client is a client interface to access messagepack server.
%%   in erlang way, this is just a OTP worker. You can set your
%%   code into your OTP supervision tree like a gen_server.
%%
%%   current status
%%     only TCP works, now rewriting as to work also UDP works.
%%
%%  <code>
%%  sample()->
%%  %just as a syntax sugar for start_link
%%  %YourModule defines receiver-callback when notification came from server.
%%   {ok, Pid}=mp_client:connect(Identifier, YourModule, [Address, Port], [tcp]),
%%   mp_client:call(Identifier, somemethod, [1,2]), % returns {ok, 3}
%%   mp_client:call_async(Identifier, somemethod, [1,2]),
%%   receive
%%       {ok, Answer} -> ok;% maybe 3
%%       _ -> error
%%   after 1024 -> timeout end
%%   mp_client:close(Pid).
%%  </code>
%%% @end
%%% Created : 26 Aug 2010 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(gen_msgpack_rpc_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% my_first_case(_Config) ->
%%     {ok, _Pid}=mp_client:connect(localhost,65500),
%%     {ok, Result}=mp_client:call(42, hello, []),
%%     true=is_list(Result),
%%     ok=mp_client:close().

%% my_second_case(_)->
%%     {ok, _}=mp_client:connect({local, hoge}, localhost,65500),
%%     {ok, Result}=mp_client:call(hoge,42, hello, []),
%%     true=is_list(Result),
%%     {ok, 7}=mp_client:call(hoge,43, add, [3,4]),
%%     ok=mp_client:close(hoge).

%% case_add(_Config)->
%%     Pairs=[{5,5}, {0,0}, {234, 2}, {213456789, -3}, {234, -23}, {-1,1}, {1,-1}, {-1,-1},
%% 	  {-2000, 2000}, {2000, -2000}, {234, -234}],
%%     {ok, _Pid}=mp_client:connect({local,add}, localhost,65500),
%%     {ok, _Result}=mp_client:call(add, 42, hello, []),
%%     lists:map( fun({L,R})-> S=L+R, {ok,S}=mp_client:call(add, (L+42), add, [L,R])  end, Pairs ),
%%     {error, {<<"no such func">>,nil}}=mp_client:call(add, 890, no_such_func, []),
%%     mp_client:close(add).

%% my_test()->
%%     ok=sample_app:start(),
%%     ok=my_first_case(nil),
%%     ok=my_second_case(nil),
%%     ok=case_add(nil),
%%     ok=sample_app:stop().
    
%%     {ok,Pid}=mp_client:connect(localhost,65500),
%%     {ok,_Reply}=mp_client:call(Pid, hoge, []),
%%     mp_client:close().

-endif.
