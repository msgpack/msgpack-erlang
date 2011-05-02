%%%-------------------------------------------------------------------
%%% File    : msgpack_util.erl
%%% Author  : UENISHI Kota <kuenishi@gmail.com>
%%% Description : 
%%%
%%% Created :  2 May 2011 by UENISHI Kota <kuenishi@gmail.com>
%%%-------------------------------------------------------------------
-module(msgpack_util).

-export([pppop/2, append_sup/1, append_srv/1]).
-export([start/0, get_callid/0, insert/1, call_done/1, lookup/1, stop/0]).

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

-spec start()-> ok.
start()->
    ?MODULE=ets:new(?MODULE, [set,public,named_table]), ok.

-spec stop()-> ok.
stop()->
    true=ets:delete(?MODULE), ok.

get_callid()->
    Cand = random:uniform(16#FFFFFFF),
    case ets:insert_new(?MODULE, {Cand,Cand}) of
	true -> Cand;
	false -> get_callid()
    end.

insert(ResultTuple)->
    true=ets:insert(?MODULE, ResultTuple), ok.

call_done(CallID)->
    true=ets:delete(?MODULE, CallID), ok.

lookup(CallID)->
    ets:lookup(?MODULE, CallID).
