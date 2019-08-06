# -*- mode: dockerfile -*-

# Build the host instance
FROM alpine:3.10.1 AS build
RUN apk add --update git gcc make libc-dev

WORKDIR /usr/src
COPY ./udp.c /usr/src/
COPY ./Makefile /usr/src/
RUN make udp.bin

FROM alpine:3.10.1
COPY --from=build /usr/src/udp.bin /usr/local/bin/udp-server
WORKDIR /app

CMD ["/usr/local/bin/udp-server"]
