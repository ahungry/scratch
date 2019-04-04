% Grab first element of a list
:- module first.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

:- func first(list(int)) = int.
first(List) = ( if H = head(List) then H else 0 ).

main(!IO) :-
  F = first([1,2,3]),
  io.format("head is %d\n", [i(F)], !IO),
  N = ( if H = head([a,b,c]) then H else z ),
  io.format("head is %c\n", [c(N)], !IO).
