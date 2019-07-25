# Docker Benchmarks for different images

To get a bare minimum node/npm/yarn environment setup, the following
timings were observed:

## Building

### alpine:3.10.1 (node v10)
Average time among a few runs: 30 seconds total @ 30 to 50 MB ram
during build

### Ubuntu 18.04 (node v8)
Average time among a few runs: 200 seconds total @ 60 to 100 MB ram during build

### Ubuntu 19.04 (node v10)
Average time among a few runs: 200 seconds total @ 60 to 100 MB ram during build

## Container resource consumption while running + space taken by image

### alpine:3.10.1 (node v10)
60MB ram / 18 PIDs / 0.02% CPU
Image: 64.9MB disk usage

### Ubuntu 18.04 (node v8)
65MB ram / 20 PIDs / 0.02% CPU
Image: 440MB disk usage

### Ubuntu 19.04 (node v10)
65MB ram / 20 PIDs / 0.02% CPU
Image: 440MB disk usage

### alpine:php-7 image with nginx / php-fpm (php 7.3.5)
45MB ram / 16 PIDs / 0.02% CPU
Image: 106MB disk usage

### alpine:3.10.1 with custom C http server
< 1 MB ram / 1 PIDs / 0.00% CPU
Image: 5MB disk usage

## Container performance under load

siege -r1000 -c5 http://localhost:3001 (or 3002)

## alpine:3.10.1 (node v10)

Transactions:                   5000 hits
Availability:                 100.00 %
Elapsed time:                   4.29 secs
Data transferred:               0.06 MB
Response time:                  0.00 secs
Transaction rate:            1165.50 trans/sec
Throughput:                     0.01 MB/sec
Concurrency:                    4.85
Successful transactions:        5000
Failed transactions:               0
Longest transaction:            0.02
Shortest transaction:           0.00

## ubuntu:19.04 (node v10)

Transactions:                   5000 hits
Availability:                 100.00 %
Elapsed time:                   3.20 secs
Data transferred:               0.06 MB
Response time:                  0.00 secs
Transaction rate:            1562.50 trans/sec
Throughput:                     0.02 MB/sec
Concurrency:                    4.83
Successful transactions:        5000
Failed transactions:               0
Longest transaction:            0.01
Shortest transaction:           0.00

## alpine php 7.3

Transactions:                   5000 hits
Availability:                 100.00 %
Elapsed time:                   2.36 secs
Data transferred:               0.15 MB
Response time:                  0.00 secs
Transaction rate:            2118.64 trans/sec
Throughput:                     0.06 MB/sec
Concurrency:                    4.63
Successful transactions:        5000
Failed transactions:               0
Longest transaction:            0.01
Shortest transaction:           0.00

## alpine:3.10.1 with custom C http server

Transactions:                   5000 hits
Availability:                 100.00 %
Elapsed time:                   1.54 secs
Data transferred:               0.03 MB
Response time:                  0.00 secs
Transaction rate:            3246.75 trans/sec
Throughput:                     0.02 MB/sec
Concurrency:                    4.40
Successful transactions:        5000
Failed transactions:               0
Longest transaction:            0.01
Shortest transaction:           0.00
