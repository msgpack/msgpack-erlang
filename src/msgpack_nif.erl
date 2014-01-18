%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
%%
%% Created : 17 Feb 2013
%%
-module(msgpack_nif).
-on_load(init/0).

-export([init/0, pack/1, unpack_stream/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init()->
    SoName =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        filename:join([filename:dirname(Filename),"../priv", "msgpack_drv"]);
                    _ ->
                        filename:join("../priv", "msgpack_drv")
                end;
            Dir ->
                filename:join(Dir, "msgpack_drv")
        end,
    erlang:load_nif(SoName, 0).

%% @doc 'pack' in old spec using nif.
-spec pack(msgpack:object()) -> binary() | {error, {badarg, term()}}.
pack(_) ->
    throw(nif_not_loaded).

-spec unpack_stream(binary()) -> {msgpack:object(), binary()} | {error, incomplete} | {error, {badarg, term()}}.
unpack_stream(_) ->
    throw(nif_not_loaded).

-ifdef(TEST).

mini_test()->
    ?assertEqual({ok, []}, msgpack:unpack(msgpack_nif:pack([]))),
    ?assertEqual({235, <<>>}, msgpack_nif:unpack_stream(msgpack:pack(235))),
    Bin = << (msgpack:pack(34))/binary, 342>>,
    ?assertEqual({34, <<342>>},
                 msgpack_nif:unpack_stream(Bin)),
    ?assertEqual({[23,323,344], <<>>},
                 msgpack_nif:unpack_stream(msgpack:pack([23,323,344]))),
    ?assertEqual({{[{23,23}]}, <<>>},
                 msgpack_nif:unpack_stream(msgpack:pack({[{23,23}]}))),

    %% ?assertEqual({ok, {[]}}, msgpack_nif:unpack(msgpack_nif:pack({[]}))),
    ?assertEqual({ok, 2345}, msgpack:unpack(msgpack_nif:pack(2345))),
    ?assertEqual({ok, [2345]}, msgpack:unpack(msgpack_nif:pack([2345]))),

    ?assertEqual({ok, {[{34,45}]}},
                 msgpack:unpack(msgpack_nif:pack({[{34,45}]}))),
    ok.

fail_test() ->
    ?assertException(error, nif_with_ext,
                     msgpack:pack([], [{use_nif,true},{ext, msgpack_term}])).
%%    ok.

-endif.
