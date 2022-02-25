# -*- mode: dockerfile -*-
FROM alpine:latest

RUN apk add --update nodejs npm

EXPOSE 8080
COPY server.js .
CMD node server.js
