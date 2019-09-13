# results

It took ~0.006 seconds of overhead to use the runtime approach,
compared to 4.228 seconds of typescript time.  This means it is around
1000x faster than the Typescript compiler.

## Generating js out of ts with 5k comps

### Compile time

```sh
time tsc bench-ts.ts > bench-ts.js
tsc bench-ts.ts > bench-ts.js  5.43s user 0.06s system 129% cpu 4.228 total
```

### Run time

```sh
time node bench-ts.js
node bench-ts.js  0.07s user 0.01s system 100% cpu 0.081 total
```

## Using the "runtime" type checking system

### Run time

```sh
time node rt.js
node rt.js  0.09s user 0.00s system 106% cpu 0.087 total
```

## Findings when we introduce an error by hand

```sh
time node rt.js
5001
/home/mcarter/src/scratch/composed-types/rt.js:7
const err = (_i, _o) => { throw new Error("Incompatible types!") }

Error: Incompatible types!
node rt.js  0.08s user 0.01s system 106% cpu 0.087 total
time tsc bench-ts.ts
bench-ts.ts:5008:19 - error TS2345: Argument of type 'SFn' is not assignable to parameter of type 'NFn'.
tsc bench-ts.ts  5.63s user 0.05s system 131% cpu 4.322 total
```
