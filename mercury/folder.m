% Fold up a list
:- module folder.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

:- pred folder(list(int)::in, int::out) is det.
folder(List, Sum) :-
  list.foldl(folder_time, List, 0, Sum).

:- pred folder_time(int::in, int::in, int::out) is det.
folder_time(N, In, Out) :-
  Out = N + In.

:- func ffun(list(int)) = int.
ffun(List) = (
  if
    list.foldl(folder_time, List, 0, Sum)
  then
    Sum
  else
    0
).

main(!IO) :-
  folder([1,2,3], Sum),
  io.format("Sum of 1,2,3 is %d\n", [i(Sum)], !IO),
  io.format("Sum of 1,2,3 is %d\n", [i(ffun([1,2,3]))], !IO).