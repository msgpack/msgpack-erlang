%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
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
%%
%% Created : 18 Feb 2013 by UENISHI Kota <kuenishi@gmail.com>

-module(bench_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PCNT, 5).
-define(CNT, 10000).

test_data()->
    [true, false, nil,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     -16#80000001,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, nil, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42
    ].

%%     [{1,1}, {2,2}, {3,3}],
%%     [{1,1}, {2,2}, {3,3}, {4,4}, {5,5}],

benchmark0_test()->
    Data=[test_data() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", msgpack:pack(Data, [jiffy])),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S, [jiffy])),
    ?debugFmt("for ~p KB test data(jiffy).", [byte_size(S) div 1024]).

benchmark1_test()->
    Data=[test_data() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", msgpack:pack(Data, [jsx])),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S, [jsx])),
    ?debugFmt("for ~p KB test data(jsx).", [byte_size(S) div 1024]).

%% benchmark2_test()->
%%     Data=[test_data() || _ <- lists:seq(0, ?CNT)],
%%     S=?debugTime("  serialize", msgpack_nif:pack(Data)),
%%     {ok, Data}=?debugTime("deserialize", msgpack_nif:unpack(S)),
%%     ?debugFmt("for ~p KB test data(nif).", [byte_size(S) div 1024]).

benchmark3_test()->
    Data=[test_data() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", term_to_binary(Data)),
    Data=?debugTime("deserialize", binary_to_term(S)),
    ?debugFmt("for ~p KB test data(t2b/b2t).", [byte_size(S) div 1024]).

multirunner(What, Pack, Unpack) ->
    Self = self(),
    Data=[test_data() || _ <- lists:seq(0, ?CNT)],
    Packed = Pack(Data),
    Size = byte_size(Packed) div 1024,
    [ spawn(fun() ->
                    {T, _} = timer:tc(Pack, [Data]),
                    Self ! {p0, N, T}
            end)|| N <- lists:seq(1, ?PCNT)],
    TimesPack = [receive
                     {p0, N, Time} ->
                         Time
                 end || N <- lists:seq(1, ?PCNT)],
    TotalPack = lists:foldl(fun(N, Acc) ->
                                    Acc + N
                            end, 0, TimesPack),

    ?debugFmt("   serialize: ~.3f s", [TotalPack/1000/1000/?PCNT]),
    [ spawn(fun() ->
                    {T, _} = timer:tc(Unpack, [Packed]),
                    Self ! {p0, N, T}
            end) || N <- lists:seq(1, ?PCNT)],
    TimesUnpack = [receive
                       {p0, N, Time} ->
                           Time
                   end || N <- lists:seq(1, ?PCNT)],
    TotalUnpack = lists:foldl(fun(N, Acc) ->
                                      Acc + N
                              end, 0, TimesUnpack),
    ?debugFmt(" deserialize: ~.3f s", [TotalUnpack/1000/1000/?PCNT]),
    ?debugFmt("for ~p KB test data(~s x ~p).", [Size, What, ?PCNT]),
    ok.



benchmark_p0_test_() ->
    {timeout, 600,
     ?_assertEqual(ok,
                   multirunner("jiffy",
                               fun(Data) ->
                                       msgpack:pack(Data, [jiffy])
                               end,
                               fun(Data) ->
                                       msgpack:unpack(Data, [jiffy])
                               end))}.

benchmark_p1_test_() ->
    {timeout, 600,
     ?_assertEqual(ok,
                   multirunner("jsx",
                               fun(Data) ->
                                       msgpack:pack(Data, [jsx])
                               end,
                               fun(Data) ->
                                       msgpack:unpack(Data, [jsx])
                               end))}.

%% benchmark_p2_test_() ->
%%     {timeout, 600,
%%      ?_assertEqual(ok,
%%                    multirunner("nif",
%%                                fun(Data) ->
%%                                        msgpack_nif:pack(Data)
%%                                end,
%%                                fun(Data) ->
%%                                        msgpack_nif:unpack(Data)
%%                                end))}.

benchmark_p3_test_() ->
    {timeout, 600,
     ?_assertEqual(ok,
                   multirunner("t2b/b2t",
                               fun(Data) ->
                                       term_to_binary(Data)
                               end,
                               fun(Data) ->
                                       binary_to_term(Data)
                               end))}.

-endif.
