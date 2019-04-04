% Define the module
:- module hello.

% What is public for users
:- interface.

% Necessary things we have to include
:- import_module io.

% predicate destructive input, unique output is deterministic
:- pred main(io::di, io::uo) is det.

% Private implementation details not visible outside
:- implementation.

% Clause defining main
main(!IO) :-
    io.write_string("Hello, David!\n", !IO),
    io.nl(!IO).
