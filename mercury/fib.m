:- module fib.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.
% :- import_module string.
:- import_module int, list, string.

:- pred fib(int::in, int::out) is det.

% As a predicate
fib(N, X) :-
  (
    if
      N =< 2
    then
      X = 1
    else
      fib(N - 1, A), fib(N - 2, B), X = A + B
  ).

% As a function with a body
:- func fibf(int) = int.

fibf(N) = X :-
  (
    if   N =< 2
    then X = 1
    else X = fibf(N - 1) + fibf(N - 2)
).

% As a function with just a head
:- func fibh(int) = int.

fibh(N) = ( if N =< 2 then 1 else fibh(N - 1) + fibh(N - 2)).

main(!IO) :-
  io.read_line_as_string(Result, !IO),
  (
    if
      Result = ok(String),
      string.to_int(string.strip(String), N)
    then
      io.format("fib(%d) = %d\n", [i(N), i(fibf(N))], !IO)
    else
      io.format("That isn't a number...\n", [], !IO)
  ).
