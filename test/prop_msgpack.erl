-module(prop_msgpack).
-include_lib("proper/include/proper.hrl").
-include("msgpack.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, mytype(),
        begin
            boolean(Type)
        end).

prop_pack() ->
    ?FORALL(Term, msgpack_term(),
        begin
            Bin = msgpack:pack(Term),
            is_binary(Bin)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
msgpack_term() ->
    frequency(
      [
       {10, integer()},
       {10, float()},
       {10, boolean()},
       {10, binary()},
       {10, utf8()},
       {2, ?LAZY(list(msgpack_term()))},
       {1, ?LAZY(map(msgpack_term(), msgpack_term()))}
      ]).


mytype() ->
    term().
