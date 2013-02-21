-module(msgpack_props).

-include_lib("proper/include/proper.hrl").

-import(msgpack_proper, [choose_type/0]).

prop_type() ->
    numtests(128,
        ?FORALL(Term, choose_type(),
                begin
                    Binary = msgpack:pack(Term),
                    {ok, Term1} = msgpack:unpack(Binary),
                    Term =:= Term1
                end)).


choose_reserved() ->
    oneof([<<16#C4>>,
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
    numtests(128,
        ?FORALL({ReservedByte, Term}, {choose_reserved(), choose_type()},
                begin
                    Binary = <<ReservedByte/binary, (msgpack:pack(Term))/binary>>,
                    {ok, Term1} = msgpack:unpack(Binary),
                    Term =:= Term1
                end)).
