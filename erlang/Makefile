default: hw.beam

run: hw.beam
	erl -s hw start -noshell ./hw.beam -s init stop

hw.beam:
	erlc hw.erl
