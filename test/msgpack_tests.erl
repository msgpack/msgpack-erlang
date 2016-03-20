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
%% Created : 26 Apr 2011 by UENISHI Kota <uenishi.kota@lab.ntt.co.jp>

-module(msgpack_tests).

-import(msgpack, [pack/2, unpack/2, pack/1, unpack/1]).

-include_lib("eunit/include/eunit.hrl").

test_data() ->
    [true, false, null,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, null, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42].

test_data_jsx()->
    test_data() ++ [[{}]].

test_data_jiffy()->
    test_data() ++ [ {[]} ].

test_data_map()->
    test_data() ++ [ #{} ].

-ifdef(DO_MSGPACK_CROSSLANG_TEST).

compare_all([], [])-> ok;
compare_all([],  R)-> {toomuchrhs, R};
compare_all(L,  [])-> {toomuchlhs, L};
compare_all([LH|LTL], [RH|RTL]) ->
    ?assertEqual(LH, RH),
    compare_all(LTL, RTL).

port_receive(Port) ->
    port_receive(Port, <<>>).
port_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} -> port_receive(Port, <<Acc/binary, Data/binary>>);
        {Port, eof} -> Acc
    after 1000 -> Acc
    end.

port_map_test()->
    Tests = test_data_map(),
    ?assertEqual({[Tests],<<>>}, msgpack:unpack(msgpack:pack([Tests], [{map_format,map}]), [{map_format,map}])),

port_jiffy_test()->
    Tests = test_data_jiffy(),
    ?assertEqual({[Tests],<<>>}, msgpack:unpack(msgpack:pack([Tests], [{map_format,jiffy}]), [{map_format,jiffy}])),

                                                %    Port = open_port({spawn, "ruby ../test/crosslang.rb"}, [binary, eof]),
                                                %    true = port_command(Port, msgpack:pack(Tests)),
                                                %    ?assertEqual({Tests, <<>>}, msgpack:unpack(port_receive(Port))),
                                                %    port_close(Port).
    ok.


port_jsx_test()->
    Tests = test_data_jsx(),
    ?assertEqual({[Tests],<<>>}, msgpack:unpack(msgpack:pack([Tests], [{map_format,jsx}]), [{map_format,jsx}])),

                                                %    Port = open_port({spawn, "ruby ../test/crosslang.rb"}, [binary, eof]),
                                                %    true = port_command(Port, msgpack:pack(Tests)),
                                                %    ?assertEqual({Tests, <<>>}, msgpack:unpack(port_receive(Port))),
                                                %    port_close(Port).
    ok.

unknown_test_freezed_test_dont_do_this()->
    Port = open_port({spawn, "ruby testcase_generator.rb"}, [binary, eof]),
    Tests = [0, 1, 2, 123, 512, 1230, 678908,
             -1, -23, -512, -1230, -567898,
             <<"hogehoge">>, <<"243546rf7g68h798j">>,
             123.123,
             -234.4355, 1.0e-34, 1.0e64,
             [23, 234, 0.23],
             [0,42,<<"sum">>, [1,2]], [1,42, null, [3]],
             [{1,2},{<<"hoge">>,null}], % map
             -234, -50000,
             42
            ],
    ?assertEqual(ok, compare_all(Tests, msgpack:unpack_all(port_receive(Port)))),
    port_close(Port).

-endif.

issue_jsx_5_test() ->
    %% {'type':"workers", 'data':[{'workerid': "std.1", 'slots':[] }]}
    Term = [
            {<<"type">>, <<"workers">>},
            {<<"data">>,[
                         [{<<"workerid">>, <<"std.1">>}, {<<"slots">>, []}]
                        ]
            }
           ],
    Encoded = msgpack:pack(Term, [{map_format,jsx}, {spec,new}]),
    Bin0 = <<130,196,4,116,121,112,101,196,7,119,111,114,107,101,114,115,
             196,4,100,97,116,97,145,130,196,8,119,111,114,107,101,114,105,100,
             196,5,115,116,100,46,49,196,5,115,108,111,116,115,160>>,

    ?assertEqual(Bin0, Encoded),

    {ok, Decoded} = msgpack:unpack(Bin0, [{map_format,jsx}, {spec,new}]),
    ?assertEqual(Term, Decoded).


issue_jiffy_5_test() ->
    %% {'type':"workers", 'data':[{'workerid': "std.1", 'slots':[] }]}
    Term = {[
             {<<"type">>, <<"workers">>},
             {<<"data">>,[
                          {[{<<"workerid">>, <<"std.1">>},{<<"slots">>, []}]}
                         ]
             }
            ]},
    Encoded = msgpack:pack(Term, [{map_format,jiffy}, {spec,new}]),
    Bin0 = <<130,196,4,116,121,112,101,196,7,119,111,114,107,101,114,115,
             196,4,100,97,116,97,145,130,196,8,119,111,114,107,101,114,105,100,
             196,5,115,116,100,46,49,196,5,115,108,111,116,115,160>>,
    ?assertEqual(Bin0, Encoded),

    {ok, Decoded} = msgpack:unpack(Bin0, [{map_format,jiffy}, {spec,new}]),
    ?assertEqual(Term, Decoded).


issue_27_test_() ->
    [
     %% null(jiffy) => null(msgpack) => null(jsx)
     ?_assertEqual({ok, null},
                   msgpack:unpack(msgpack:pack(null, [{map_format,jiffy}]), [{map_format,jsx}])),

     %% null(jiffy) => null(msgpack) => null(jiffy)
     ?_assertEqual({ok, null},
                   msgpack:unpack(msgpack:pack(null, [{map_format,jiffy}]), [{map_format,jiffy}])),


     %% null(jsx) => null(msgpack) => null(jiffy)
     ?_assertEqual({ok, null},
                   msgpack:unpack(msgpack:pack(null, [{map_format,jsx}]), [{map_format,jiffy}]))].

string_test() ->
    {ok, CWD} = file:get_cwd(),
    Path = CWD ++ "/test/utf8.txt",
    %% ?debugVal(Path),
    {ok, UnicodeBin} = file:read_file(Path),
    String = unicode:characters_to_list(UnicodeBin),
    MsgpackStringBin = msgpack:pack(String),
    {ok, String} = msgpack:unpack(MsgpackStringBin).

default_test_() ->
    [
     {"pack",
      fun() ->
              Map = #{1=>2},
              ?assertEqual(pack(Map, [{map_format, map}]), pack(Map))
      end},
     {"unpack",
      fun() ->
              Map = {[{1,2}]},
              Binary = pack(Map, [{map_format, map}]),
              ?assertEqual(unpack(Binary, [{map_format, map}]), unpack(Binary))
      end}
    ].

unpack_test_() ->
    [
     {"not binary",
      ?_assertEqual({error, {badarg, []}}, unpack([]))},

     {"incomplete: null binary",
      ?_assertEqual({error, incomplete}, unpack(<<>>))},

     {"incomplete: unknown binary",
      ?_assertEqual({error, incomplete}, unpack(<<16#DA>>))}
    ].

array_test_()->
    [
     {"length 16",
      fun() ->
              List = lists:seq(0, 16),
              Binary = pack(List),
              ?assertEqual({ok, List}, unpack(Binary))
      end},
     {"length 32",
      fun() ->
              List = lists:seq(0, 16#010000),
              Binary = pack(List),
              ?assertEqual({ok, List}, unpack(Binary))
      end},
     {"empty",
      fun() ->
              EmptyList = [],
              Binary = pack(EmptyList),
              ?assertEqual({ok, EmptyList}, unpack(Binary))
      end}
    ].

-ifndef(without_map).
map_test_()->
    [
     {"map <=> jsx",
      fun() ->
              JSXMap = [ {X, X * 2} || X <- lists:seq(0, 16) ],
              BinaryJSX = pack(JSXMap, [{map_format,jsx}]),
              Map = maps:from_list(JSXMap),
              Binary = pack(Map, [{map_format,map}]),
              ?assertEqual(BinaryJSX, Binary)
      end},

     {"pack map without {map_format,map}",
      fun() ->
	      Map = maps:from_list([ {X, X * 2} || X <- lists:seq(0, 16) ]),
	      Binary = pack(Map),
	      ?assertEqual({ok,Map}, unpack(Binary, [{map_format,map}]))
      end},

     {"map length 16",
      fun() ->
              Map = maps:from_list([ {X, X * 2} || X <- lists:seq(0, 16) ]),
              Binary = pack(Map, [{map_format,map}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,map}]))
      end},
     {"map length 32",
      fun() ->
              Map = maps:from_list([ {X, X * 2} || X <- lists:seq(0, 16#010000) ]),
              Binary = pack(Map, [{map_format,map}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,map}]))
      end},
     {"map empty",
      fun() ->
              EmptyMap = maps:new(),
              Binary = pack(EmptyMap, [{map_format,map}]),
              ?assertEqual({ok, EmptyMap}, unpack(Binary, [{map_format,map}]))
      end}].
-endif.


jiffy_jsx_test_() ->
    [{"jiffy length 16",
      fun() ->
              Map = {[ {X, X * 2} || X <- lists:seq(0, 16) ]},
              Binary = pack(Map, [{map_format,jiffy}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,jiffy}]))
      end},
     {"jiffy length 32",
      fun() ->
              Map = {[ {X, X * 2} || X <- lists:seq(0, 16#010000) ]},
              Binary = pack(Map, [{map_format,jiffy}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,jiffy}]))
      end},
     {"jiffy empty",
      fun() ->
              EmptyMap = {[]},
              Binary = pack(EmptyMap, [{map_format,jiffy}]),
              ?assertEqual({ok, EmptyMap}, unpack(Binary, [{map_format,jiffy}]))
      end},
     {"jsx length 16",
      fun() ->
              Map = [ {X, X * 2} || X <- lists:seq(0, 16) ],
              Binary = pack(Map, [{map_format,jsx}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,jsx}]))
      end},
     {"jsx length 32",
      fun() ->
              Map = [ {X, X * 2} || X <- lists:seq(0, 16#010000) ],
              Binary = pack(Map, [{map_format,jsx}]),
              ?assertEqual({ok, Map}, unpack(Binary, [{map_format,jsx}]))
      end},
     {"jsx empty",
      fun() ->
              EmptyMap = [{}],
              Binary = pack(EmptyMap, [{map_format,jsx}]),
              ?assertEqual({ok, EmptyMap}, unpack(Binary, [{map_format,jsx}]))
      end}
    ].

int_test_() ->
    [
     {"negative fixnum",
      fun() ->
              Term = -32,
              Binary = pack(Term),
              ?assertEqual(1, byte_size(Binary)),
              ?assertEqual({ok, Term}, unpack(Binary))
      end},
     {"int 8",
      fun() ->
              Term = -33,
              Binary = pack(Term),
              ?assertEqual(2, byte_size(Binary)),
              ?assertEqual({ok, Term}, unpack(Binary)),

              Term2 = -128,
              Binary2 = pack(Term2),
              ?assertEqual(2, byte_size(Binary2)),
              ?assertEqual({ok, Term2}, unpack(Binary2))
      end},
     {"int 16",
      fun() ->
              Term = -129,
              Binary = pack(Term),
              ?assertEqual(3, byte_size(Binary)),
              ?assertEqual({ok, Term}, unpack(Binary)),

              Term2 = -16#8000,
              Binary2 = pack(Term2),
              ?assertEqual(3, byte_size(Binary2)),
              ?assertEqual({ok, Term2}, unpack(Binary2))
      end},
     {"int 32",
      fun() ->
              Term = -16#8001,
              Binary = pack(Term),
              ?assertEqual(5, byte_size(Binary)),
              ?assertEqual({ok, Term}, unpack(Binary)),

              Term2 = -16#80000000,
              Binary2 = pack(Term2),
              ?assertEqual(5, byte_size(Binary2)),
              ?assertEqual({ok, Term2}, unpack(Binary2))
      end},
     {"int 64",
      fun() ->
              Term = -16#80000001,
              Binary = pack(Term),
              ?assertEqual(9, byte_size(Binary)),
              ?assertEqual({ok, Term}, unpack(Binary)),

              Term2 = -16#8000000000000000,
              Binary2 = pack(Term2),
              ?assertEqual(9, byte_size(Binary2)),
              ?assertEqual({ok, Term2}, unpack(Binary2))
      end}
    ].

error_test_()->
    [
     {"badarg atom",
      ?_assertEqual({error, {badarg, atom}},
                    pack(atom, [{allow_atom, none}]))},
     {"badarg tuple",
      fun() ->
              Term = {"hoge", "hage", atom},
              ?assertEqual({error, {badarg, Term}},
                           pack(Term, [{allow_atom, none}]))
      end},
     {"badarg too big int",
      ?_assertEqual({error, {badarg, -16#8000000000000001}},
                     pack(-16#8000000000000001))},
     {"badarg too big uint",
      ?_assertEqual({error, {badarg, 16#10000000000000000}},
                     pack(16#10000000000000000))}
    ].

binary_test_() ->
    [
     {"0 byte",
      fun() ->
              Binary = pack(<<>>),
              ?assertEqual({ok, <<>>}, unpack(Binary))
      end}
    ].

%% long_binary_test_()->
%%     [
%%         {"long binary",
%%             fun() ->
%%                     A = pack(1),
%%                     B = pack(10),
%%                     C = pack(100),
%%                     ?assertEqual({[1,10,100], <<>>},
%%                                  unpack(list_to_binary([A, B, C])))
%%             end}
%%     ].

-define(PCNT, 5).
-define(CNT, 10000).

benchmark0_test()->
    Data=[test_data_jiffy() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", msgpack:pack(Data, [{map_format, jiffy}])),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S, [{map_format, jiffy}])),
    ?debugFmt("for ~p KB test data(jiffy).", [byte_size(S) div 1024]).

benchmark1_test()->
    Data=[test_data_jsx() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", msgpack:pack(Data, [{map_format, jsx}])),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S, [{map_format, jsx}])),
    ?debugFmt("for ~p KB test data(jsx).", [byte_size(S) div 1024]).

benchmark2_test()->
    Data=[test_data_map() || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", msgpack:pack(Data)),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S)),
    ?debugFmt("for ~p KB test data(maps).", [byte_size(S) div 1024]).

benchmark3_test()->
    Data=[test_data() ++ [null] || _ <- lists:seq(0, ?CNT)],
    S=?debugTime("  serialize", term_to_binary(Data)),
    Data=?debugTime("deserialize", binary_to_term(S)),
    ?debugFmt("for ~p KB test data(t2b/b2t).", [byte_size(S) div 1024]).

multirunner(What, Pack, Unpack) ->
    Self = self(),
    Nil = null,
                   
    Data=[test_data() ++ [Nil] || _ <- lists:seq(0, ?CNT)],
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
    ?debugFmt("~.3f s/~.3f s for ser/de ~p KB test data(~s x ~p).",
              [TotalPack/1000/1000/?PCNT, TotalUnpack/1000/1000/?PCNT,
               Size, What, ?PCNT]),
    ok.

benchmark_p0_test_() ->
    [{timeout, 600,
      ?_assertEqual(ok,
                    multirunner("jiffy",
                                fun(Data) ->
                                        msgpack:pack(Data, [{map_format, jiffy}])
                                end,
                                fun(Data) ->
                                        msgpack:unpack(Data, [{map_format, jiffy}])
                                end))},
     {timeout, 600,
      ?_assertEqual(ok,
                    multirunner("jsx",
                                fun(Data) ->
                                        msgpack:pack(Data, [{map_format, jsx}])
                                end,
                                fun(Data) ->
                                        msgpack:unpack(Data, [{map_format, jsx}])
                                end))},
     {timeout, 600,
      ?_assertEqual(ok,
                    multirunner("maps",
                                fun msgpack:pack/1,
                                fun msgpack:unpack/1))},
     {timeout, 600,
      ?_assertEqual(ok,
                    multirunner("t2b/b2t",
                                fun erlang:term_to_binary/1,
                                fun erlang:binary_to_term/1))}].
