default: hw.beam

run: hw.beam
	erl -s hw start -noshell ./hw.beam -s init stop

hw.beam:
	erlc hw.erl

test-promise: promise.beam
	erl -s promise test -noshell ./promise.beam -s init stop

promise.beam:
	erlc promise.erl
