#!/bin/sh

echo -ne "Content-Type: application/json\n\n"

# env
if [ "$REQUEST_METHOD" != 'POST' ]; then
    echo "select * from users" | sqlite3 -csv /tmp/users.db
    echo fin
    exit 0
fi

json=$(cat <&0)

user=$(echo $json | jq .user | sed -e 's/"//g')
pass=$(echo $json | jq .pass | sed -e 's/"//g')

result='false'

# Validation
if [ -z "$user" -o -z "$pass" -o '"null"' = "$user" -o '"null"' = "$pass" ]; then
    result='false'
else
    result='true'
fi

# Persistence
sqlite3 /tmp/users.db "INSERT INTO users (user, pass) VALUES ('$user', '$pass')" 2>&1

cat <<EOF
{"success": $result, "user": "${user}", "pass": "${pass}"}
EOF

exit 0
