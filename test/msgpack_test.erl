%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%% @copyright (C) 2011, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 26 Apr 2011 by UENISHI Kota <uenishi.kota@lab.ntt.co.jp>
%%%-------------------------------------------------------------------
-module(msgpack_test).

-ifdef(DO_MSGPACK_CROSSLANG_TEST).

-include_lib("eunit/include/eunit.hrl").

test_data()->
    [true, false, nil,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, nil, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42,
     {[]}, {hoge}
    ].

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

port_test()->
    Tests = test_data(),
    ?assertEqual({[Tests],<<>>}, msgpack:unpack(msgpack:pack([Tests]))),

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
	     [0,42,<<"sum">>, [1,2]], [1,42, nil, [3]],
	     {[{1,2},{<<"hoge">>,nil}]}, % map
	     -234, -50000,
	     42
	    ],
    ?assertEqual(ok, compare_all(Tests, msgpack:unpack_all(port_receive(Port)))),
    port_close(Port).

-endif.
