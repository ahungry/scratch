%% -*- erlang-indent-level: 2 -*-

%% setting up a cluster:
%% https://stackoverflow.com/questions/49827564/how-to-connect-two-erlang-nodes-running-on-different-host-machines-in-the-networ#49835678

%% Compile directly with "c(hw)."

-module(hw).
-author("Me").

-export([
start/0,
do_points/0,
do_pattern_match/0,
make_tagged_tuple/0,
list_comprehensions/0
]).

%% Import some file stuff, common list: module things.
-import(io, [format/1]).

%% Sample of a macro (text substitution)
-define(sub(X,Y), X-Y).

%% Same as using flags -export_all and -debug_info
%% TODO: Do not keep export_all on prod stuff.
-compile([debug_info, export_all]).

start() ->
  format("Greetings again!~n"),
  io:format("Hello World~n").

%% https://learnyousomeerlang.com/starting-out-for-real

%% clear all vars via: "f()."
%% clear a single with "f(X)."

%% Assemble two things into a tuple
do_points () ->
  X = 10,
  Y = 4,
  Point = {X,Y},
  Point.

make_tagged_tuple () ->
  {point, do_points()}.

% Extract a single thing via pattern match.
do_pattern_match () ->
  Foo = do_points(),
  {X,_} = Foo,
  X.

%% using NewList = [Exp || Generator, ..., GenN, Cond1, Cond2].
%% [{K, V} || {K, V} <- hw:list_comprehensions(), K =:= doubled ].
%% [{doubled,[2,4,6,8]}]

%% A better sample is:
%% Doubled = [X || {doubled, X} <- hw:list_comprehensions()].
list_comprehensions () ->
  Doubled = [2 * N || N <- [1,2,3,4]],
  Evens = [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0],
  [{doubled, Doubled}, {evens, Evens}].

%% reimplement a BIF using pattern matching
head([H|_]) -> H.
second([_, X|_]) -> X.

%% Another pattern matching solution with guards (after the arrow is the body)
is_adult (X) when X >= 18 -> true;
is_adult (_) -> false.

is_teen(N) when N >= 13, N =< 19 -> true;
is_teen(_) -> false.

%% guards can use and and or, but cannot use user defined functions.
is_teen_alt(N) when N < 13; N > 19 -> false;
is_teen_alt(_) -> true.

%% Erlang ifs are Guard Clauses and unique to Erlang
oh_god(N) ->
  if N =:= 2 -> might_succeed;
     true -> always_does
  end.

help_me(Animal) ->
  Talk = if Animal == cat -> "meow"
            ; Animal == beef -> "mooo"
            ; Animal == dog -> "bark"
            ; Animal == tree -> "bark"
            ; true -> "fgdadfgna"
         end,
  {Animal, "says " ++ Talk ++ "!"}.

insert(X,[]) ->
  [X];
insert(X,Set) ->
  case lists:member(X,Set) of
    true -> Set;
    false -> [X|Set]
  end.

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

%% https://learnyousomeerlang.com/recursion

%% writing a length function
len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_fac(N) -> tail_fac(N, 1).

tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

tail_len(L) -> tail_len(L, 0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, 1 + Acc).

one() -> io:format("Got a one~n"), 1.
two() -> io:format("got a two~n"), 2.

add(X, Y) -> X() + Y().

%% invoke it
test_hof() ->
  hw:add(fun one/0, fun two/0),
  hw:add(fun() -> hw:one() end, fun hw:two/0).

map(_, []) -> [];
map(F, [H|T]) ->
  [F(H)|map(F, T)].

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) -> fold(F, F(H, Start), T).

test_fold() ->
  hw:fold(fun(X, Acc) -> X + Acc end, 0, lists:seq(1, 3)).

reverse(L) ->
  fold(fun(X, Acc) -> [X|Acc] end, [], L).

map_fold(F, L) ->
  reverse(hw:fold(fun(X, Acc) -> [F(X)|Acc] end, [], L)).
