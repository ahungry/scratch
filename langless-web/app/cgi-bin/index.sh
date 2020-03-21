#!/bin/sh

# Start the response for the client.
echo -ne "Content-Type: application/json\n\n"

# env # uncomment to see the full server request headers available
if [ "$REQUEST_METHOD" != 'POST' ]; then
    echo "select * from users" | sqlite3 -csv /tmp/users.db
    echo fin
    exit 0
fi

# This scoops up the stdin and cat just prints it back out (so it lets us bind in var)
json=$(cat <&0)

# Pluck out the values we care about
user=$(echo $json | jq .user | sed -e 's/"//g')
pass=$(echo $json | jq .pass | sed -e 's/"//g')

result='false'

# Validation
if [ -z "$user" -o -z "$pass" -o '"null"' = "$user" -o '"null"' = "$pass" ]; then
    result='false'
else
    result='true'
fi

# Persistence (Sql Injection vector, duh - its just a trivial POC, do not use this)
sqlite3 /tmp/users.db "INSERT INTO users (user, pass) VALUES ('$user', '$pass')" 2>&1

cat <<EOF
{"success": $result, "user": "${user}", "pass": "${pass}"}
EOF
