#!/bin/bash

ID=$1
curl -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X PUT -d '{"language": "python", "title": "test - updated", "public": false, "files": [{"name": "main.py", "content": "print(42)"}]}' localhost:8091/snippets/$1 | jq .
