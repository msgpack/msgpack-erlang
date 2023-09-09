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
    ?FORALL(Term, msgpack_object(),
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
msgpack_object() -> term().
mytype() ->
    term().
