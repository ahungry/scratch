# -*- mode: dockerfile -*-
FROM alpine:3.10.1 AS build
RUN apk add --update gcc make libc-dev
WORKDIR /app
COPY memory-eater.c /app
COPY Makefile /app
RUN make all

FROM alpine:3.10.1
COPY --from=build /app/memory-eater /app/memory-eater
WORKDIR /app

# At 1g
#CMD ["/app/memory-eater", "1073741824"]

# Try a bit less
CMD ["/app/memory-eater", "107374124"]
