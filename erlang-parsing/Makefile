default: run

# https://www.erlang.org/doc/man/erl.html
# Needs noshell twice to be quiet?
run: lab.beam
	erl -noshell -s lab start -noshell ./lab.beam -s init stop

lab.beam:
	erlc lab.erl
