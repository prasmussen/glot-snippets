#!/bin/bash

curl -sv -H 'Content-type: application/json' -X GET localhost:8091/snippets | jq .
