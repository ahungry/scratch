#!/bin/sh

/usr/sbin/lighttpd -f /etc/lighttpd/lighttpd.conf

echo 'create table users (user text, pass text);' | sqlite3 /tmp/users.db

# Ensure db is writable
chmod 777 -R /tmp

tail -f /var/log/lighttpd/*
