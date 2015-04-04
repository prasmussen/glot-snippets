#!/bin/bash

ID=$1
curl -4 -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X PUT -d '{"token": "new-token"}' localhost:8091/admin/users/$1 | jq .
