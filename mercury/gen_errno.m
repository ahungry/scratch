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

%Ah, sorry, you can replace all of that C with:

%:- mutable(float_precision, precision, 15, ground, [untrailed, attach_to_io_state]).

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