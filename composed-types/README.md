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
