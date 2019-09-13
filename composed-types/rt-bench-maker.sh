#!/bin/bash

echo 'const Num = Symbol("n")'
echo 'const Str = Symbol("s")'

echo 'const fn = (i, o, f) => ({ i, o, f })'

echo 'const invoke = ({ f }) => (x) => f(x)'
echo 'const in_type = ({ i }) => i'
echo 'const out_type = ({ o }) => o'

echo 'const err = (_i, _o) => { throw new Error("Incompatible types!") }'
echo 'const assert_types = (f, g) => in_type(f) === out_type(g) ? true : err(in_type(f), out_type(g))'
echo 'const comp = (f, g) => assert_types(f, g) ?'
echo '  fn(out_type(g), in_type(f), (x) => invoke(f)(invoke(g)(x))) :'
echo '  false'

echo 'const add_one = fn(Num, Num, n => n + 1)'
echo 'const add2 = comp(add_one, add_one)'
echo 'const add3 = comp(add_one, add2)'

gen_fun () {
    echo 'const add'$1' = comp(add_one, add'$(($1 - 1))')'
}

for i in {4..5000}; do
    gen_fun $i
done

echo 'console.log(invoke(add'$i')(1))'
