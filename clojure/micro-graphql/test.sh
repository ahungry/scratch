#!/bin/sh

curl http://localhost:3000/gql -D- -XPOST -d "$(cat ./gql/query/dice.gql)"

