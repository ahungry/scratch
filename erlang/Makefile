# https://www.erlang.org/doc/man/erl.html
# erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell
#
# The latter will use the ENV var value if set, without it, it must be passed in.
# N=3
N?=3

default: hw.beam

run: hw.beam
	@erl -noshell -s hw start -s init stop

hw.beam: hw.erl
	erlc hw.erl

test-promise: promise.beam
	erl -s promise test -noshell ./promise.beam -s init stop

promise.beam:
	erlc promise.erl

concurrency.beam: concurrency.erl
	erlc $<

concurrency: concurrency.beam
	@erl -noshell -s concurrency start -eval 'halt().' -extra $N
