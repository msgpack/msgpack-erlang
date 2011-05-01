%%%-------------------------------------------------------------------
%%% File    : msgpack_util.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  2 May 2011 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_util).

-export([pppop/2, append_sup/1, append_srv/1]).

-spec pppop(Key::atom(), proplists:proplists())-> {Value::term(), List2::proplists:proplists()}|error.
pppop(Key,List)->
    pppop(Key,List,[]).

pppop(_, [], _)-> error;
pppop(Key,[{Key,Value}|List], Carry)-> {Value, rev_append(Carry, List)};
pppop(Key,[H|Proplist], Carry)-> pppop(Key, Proplist, [H|Carry]).

rev_append([], R)->  R;
rev_append([H|L], R) ->
    rev_append(L, [H|R]).

-spec append_sup(atom()) -> atom().
append_sup(A)-> append_str(A, "_sup").

-spec append_srv(atom()) -> atom().
append_srv(A)-> append_str(A, "_srv").

append_str(A, Str)->
    L=atom_to_list(A),
    erlang:list_to_atom(L) ++ Str.
