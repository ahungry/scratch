%% -*- erlang-indent-level: 2 -*-

%% setting up a cluster:
%% https://stackoverflow.com/questions/49827564/how-to-connect-two-erlang-nodes-running-on-different-host-machines-in-the-networ#49835678

%% Compile directly with "c(hw)."

-module(hw).
-export([
start/0,
do_points/0,
do_pattern_match/0,
make_tagged_tuple/0,
list_comprehensions/0
]).

start() ->
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
