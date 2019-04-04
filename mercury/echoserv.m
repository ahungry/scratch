:- module echoserv.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module posix__socket, posix, posix__read, text, require.
:- import_module int, string, list.

main(!IO) :-
	server(4012, !IO).

:- pred or_die(string::in, posix.result::in, io::di, io::uo) is det.
or_die(_, ok, !IO).
or_die(S, error(X), !IO) :- print(X, !IO), io.nl(!IO), error(S).

:- pred or_die(string::in, posix.result(T)::in, T::out, io::di, io::uo) is det.
or_die(_, ok(X), X, !IO).
or_die(S, error(X), _, !IO) :- io.print(X, !IO), io.nl(!IO), error(S).

:- pred server(int::in, io::di, io::uo) is det.
server(Port) -->
	socket(inet, stream, protocol(0), Res),
		or_die("socket", Res, Sock),
	bind(Sock, inet(port(Port), to_inet_addr(0,0,0,0)), Res1),
		or_die("bind", Res1),
	listen(Sock, 4, Res2),
		or_die("listen", Res2),
	accept(Sock, Res3),
		or_die("accept", Res3, Client),
	echo(Client).

:- pred echo(fd::in, io::di, io::uo) is det.
echo(Sock, !IO) :-
	text.create(4096, 0, Buf),
	echo(Sock, Buf, !IO).

:- pred echo(fd::in, text.text::di, io::di, io::uo) is det.
echo(Sock, !.Buf) -->
	posix.read.read(Sock, 4096, Res, !Buf),
		or_die("read", Res, ReadCount),
	write(Sock, ReadCount, !Buf, Res1),
		or_die("write", Res1, WriteCount),
	io.format("echoed %d bytes\n", [i(WriteCount)]),
	(
		{ WriteCount = 0 }
	->
		io.write_string("Exiting.\n")
	;
		echo(Sock, !.Buf)
	).

:- func to_inet_addr(int, int, int, int) = inet_addr.
to_inet_addr(A,B,C,D) = inet_addr(D \/ (C << 8) \/ (B << 16) \/ (A << 24)).

%------------------------------------------------------------------------------%
% Copied from posix.write.m , changed to handle the buffer as a state variable
%

%-- expanded text_header.h in this
:- pragma c_header_code("
#include <unistd.h>
#ifndef ME_TEXT_HEADER_H
#define ME_TEXT_HEADER_H
typedef struct {
	unsigned len;
	char *data;
} ME_Text;
#endif
").

:- pred write(fd, int, text, text, posix__result(int), io__state, io__state).
:- mode write(in, in, di, uo, out, di, uo) is det.
write(Fd, ToWrite, !Text, Result, !IO) :-
        write0(Fd, ToWrite, !Text, Res, !IO),
        ( Res < 0 ->
                errno(Err, !IO),
                Result = error(Err)
        ;
                 Result = ok(Res)
        ).

:- pred write0(fd, int, text, text, int, io__state, io__state).
:- mode write0(in, in, di, uo, out, di, uo) is det.
:- pragma c_code(write0(Fd::in, ToWrite::in, Text0::di, Text::uo, Res::out,
                IO0::di, IO::uo), [will_not_call_mercury, thread_safe], "{
        ME_Text *txtptr;

        txtptr = (ME_Text *) Text;

        Res = write(Fd, txtptr->data, ToWrite);

        IO = IO0; Text = Text0;
}").

-------------- next part --------------
:- module gen_errno.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module string, list.

% I release this module into the public domain
% -- the author, Julian Fondren <ayrnieu at gmail.com>
% -- er, except for what isn't mine to release.
% -- that posix.write.m stuff at the bottom surely
% -- belongs to someone.

main -->
	io.write_string(c_header),
	io.write_string(c_main_pre),
	{ errno(L) },
	write_errnos(L),
	io.write_string(c_main_post).

:- pred write_errnos(list(string)::in, io::di, io::uo) is det.
write_errnos(!.L, !IO) :-
	list.map(format_errno, !L),
	dump_errnos(!.L, !IO).

:- pred format_errno(string::in, string::out) is det.
format_errno(E, S) :-
	string.format(c_errno_check, [s(E), s(E)], S).

:- pred dump_errnos(list(string)::in, io::di, io::uo) is det.
dump_errnos([], !IO).
dump_errnos([H | T]) -->
	io.write_string(H),
	dump_errnos(T).

:- func c_header = string.
:- func c_main_pre = string.
:- func c_errno_check = string.
:- func c_main_post = string.
c_header = "
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
".

c_main_pre = "
int main (void) {
".

c_errno_check = "
#ifdef %s
  printf(""%s\\n"");
#endif
".

c_main_post = "
  return 0;
}
".

%-- Copied from posix.m
:- pred errno(list(string)::out) is det.
errno([
	"E2BIG",
	"EACCES",
	"EAGAIN",
	"EBADF",
	"EBADMSG",
	"EBUSY",
	"ECANCELED",
	"ECHILD",
	"EDEADLK",
	"EDOM",
	"EEXIST",
	"EFAULT",
	"EFBIG",
	"EINPROGRESS",
	"EINTR",
	"EINVAL",
	"EIO",
	"EISDIR",
	"EMFILE",
	"EMLINK",
	"EMSGSIZE",
	"ENAMETOOLONG",
	"ENFILE",
	"ENODEV",
	"ENOENT",
	"ENOEXEC",
	"ENOLOCK",
	"ENOMEM",
	"ENOSPC",
	"ENOSYS",
	"ENOTDIR",
	"ENOTEMPTY",
	"ENOTSUP",
	"ENOTTY",
	"ENXIO",
	"EPERM",
	"EPIPE",
	"ERANGE",
	"EROFS",
	"ESPIPE",
	"ESRCH",
	"ETIMEDOUT",
	"EXDEV"]).