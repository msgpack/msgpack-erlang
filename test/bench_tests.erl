%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kuenishi@gmail.com>
%%% @copyright (C) 2013, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2013 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(bench_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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

benchmark0_test()->
    Data=[test_data() || _ <- lists:seq(0, 10000)],
    S=?debugTime("  serialize", msgpack:pack(Data)),
    {ok, Data}=?debugTime("deserialize", msgpack:unpack(S)),
    ?debugFmt("for ~p KB test data(msgpack).", [byte_size(S) div 1024]).

benchmark1_test()->
    Data=[test_data() || _ <- lists:seq(0, 10000)],
    S=?debugTime("  serialize", msgpack_nif:pack(Data)),
    {ok, Data}=?debugTime("deserialize", msgpack_nif:unpack(S)),
    ?debugFmt("for ~p KB test data(msgpack_nif).", [byte_size(S) div 1024]).

benchmark2_test()->
    Data=[test_data() || _ <- lists:seq(0, 10000)],
    S=?debugTime("  serialize", term_to_binary(Data)),
    Data=?debugTime("deserialize", binary_to_term(S)),
    ?debugFmt("for ~p KB test data(t2b/b2t).", [byte_size(S) div 1024]).

-endif.
