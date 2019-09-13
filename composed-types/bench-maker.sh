#!/bin/bash

echo 'type NFn = (x: number) => number'
echo 'const comp = (f: NFn, g: NFn): NFn => x => f(g(x))'
echo

echo 'const add1: NFn = n => n + 1'
echo 'const add2: NFn = comp(add1, add1)'
echo 'const add3: NFn = comp(add1, add2)'

gen_fun () {
    echo 'const add'$1': NFn = comp(add1, add'$(($1 - 1))')'
}

for i in {4..5000}; do
    gen_fun $i
done

echo 'console.log(add'$i'(1))'
