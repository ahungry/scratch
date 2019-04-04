% Take in multiple arguments from CLI and output their factorials
:- module factorial.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

:- func factorial(int) = int.

factorial(N) = ( if N < 1 then 1 else factorial(N - 1) * N ).

main(!IO) :-
  io.format("Factorial of 6 is %d\n", [i(factorial(6))], !IO),
  io.progname("", ProgName, !IO),
  io.format("This program is named %s.\n", [s(ProgName)], !IO),
  io.command_line_arguments(Args, !IO),
  list.foldl2(print_arg, Args, 1, _, !IO).

:- pred print_arg(string::in, int::in, int::out, io::di, io::uo) is det.

print_arg(Arg, ArgNum, ArgNum + 1, !IO) :-
  (
    if
      string.to_int(string.strip(Arg), N)
    then
      io.format("factorial of %s is %d\n", [s(Arg), i(factorial(N))], !IO)
    else
      io.format("%s isn't a number...\n", [s(Arg)], !IO)
  ).