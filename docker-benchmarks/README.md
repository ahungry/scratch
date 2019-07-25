# Docker Benchmarks for different images

To get a bare minimum node/npm/yarn environment setup, the following
timings were observed:

## Building

### alpine:3.10.1 (node v10)
Average time among a few runs: 30 seconds total

### Ubuntu 18.04 (node v8)
Average time among a few runs: 188 seconds total @ 60MB ram during build

### Ubuntu 19.04 (node v10)
Average time among a few runs: 188 seconds total @ 60MB ram during build

## Container resource consumption while running

### alpine:3.10.1 (node v10)

### Ubuntu 18.04 (node v8)

### Ubuntu 19.04 (node v10)
