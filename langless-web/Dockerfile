FROM alpine:latest

RUN apk update && \
  apk add lighttpd jq bc sqlite

COPY ./docker/lighttpd /etc/lighttpd
COPY ./app /app

COPY ./docker/boot.sh /

WORKDIR /app

CMD /boot.sh

EXPOSE 80
