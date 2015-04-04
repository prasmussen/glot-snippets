#!/bin/bash

ID=$1
curl -4 -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X DELETE localhost:8091/snippets/$1 | jq .
