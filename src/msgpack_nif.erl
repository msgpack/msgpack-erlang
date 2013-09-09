%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
%%
%% Created : 17 Feb 2013
%%
-module(msgpack_nif).
-on_load(init/0).

-export([init/0, pack/1, unpack/1, unpack_stream/1]).

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


-spec pack(msgpack:object()) -> binary() | {error, {badarg, term()}}.
pack(_) ->
    throw(nif_not_loaded).

-spec unpack_stream(binary()) -> {msgpack:object(), binary()} | {error, incomplete} | {error, {badarg, term()}}.
unpack_stream(_) ->
    throw(nif_not_loaded).

-spec unpack(binary()) -> {ok, msgpack:object()}
                              | {error, not_just_binary} % a term deserilized, but binary remains
                              | {error, incomplete}      % too few binary to deserialize complete binary
                              | {error, {badarg, term()}}.
unpack(Data) when is_binary(Data) ->
    case unpack_stream(Data) of
        {error, _} = E -> E;
        {Term, <<>>} -> {ok, Term};
        {_, Binary} when is_binary(Binary) andalso byte_size(Binary) > 0 -> {error, not_just_binary}
    end;
unpack(Badarg) ->
    {error, {badarg, Badarg}}.

%% NOTE: nif is disabled until new C version is released.
-undef(TEST).
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

    ?assertEqual({ok, {[]}}, msgpack_nif:unpack(msgpack_nif:pack({[]}))),

    ?assertEqual({ok, 2345}, msgpack:unpack(msgpack_nif:pack(2345))),
    ?assertEqual({ok, [2345]}, msgpack:unpack(msgpack_nif:pack([2345]))),

    ?assertEqual({ok, {[{34,45}]}},
                 msgpack:unpack(msgpack_nif:pack({[{34,45}]}))),
    ok.

-endif.
