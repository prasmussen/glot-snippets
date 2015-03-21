#!/bin/bash

curl -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X POST -d '{"token": "B2FF0BF0-1055-436B-B5CB-C9B485AEE029"}' localhost:8091/admin/users | jq .
