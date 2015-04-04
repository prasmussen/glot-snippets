#!/bin/bash

curl -4 -sv -H 'Content-type: application/json' -X GET localhost:8091/snippets?owner=anonymous | jq .
