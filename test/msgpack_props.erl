-module(msgpack_props).

-include_lib("proper/include/proper.hrl").

-import(msgpack_proper, [choose_type/0]).

prop_type() ->
    numtests(300,
        ?FORALL(Term, choose_type_jsx(),
                begin
                    Binary = msgpack:pack(Term, [jsx]),
                    {ok, Term1} = msgpack:unpack(Binary, [jsx]),
                    Term =:= Term1
                end)),
    numtests(300,
        ?FORALL(Term, choose_type_jiffy(),
                begin
                    Binary = msgpack:pack(Term, [jiffy]),
                    {ok, Term1} = msgpack:unpack(Binary, [jiffy]),
                    Term =:= Term1
                end)).


choose_reserved() ->
    oneof([<<16#C1>>,
           <<16#C4>>,
           <<16#C5>>,
           <<16#C6>>,
           <<16#C7>>,
           <<16#C8>>,
           <<16#C9>>,
           <<16#D4>>,
           <<16#D5>>,
           <<16#D6>>,
           <<16#D7>>,
           <<16#D8>>,
           <<16#D9>>]).

prop_reserved() ->
    numtests(300,
        ?FORALL(Type, choose_reserved(),
                begin
                    {error, {badarg, Type1}} = msgpack:unpack(Type),
                    Type =:= Type1
                end)).
