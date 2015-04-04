#!/bin/bash

curl -4 -sv -H "Authorization: Token clumeterin" -H 'Content-type: application/json' -X GET localhost:8091/admin/users | jq .
