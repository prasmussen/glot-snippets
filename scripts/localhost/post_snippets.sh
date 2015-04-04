#!/bin/bash

curl -4 -sv -H "Authorization: Token C32BE915-38E8-486C-96DB-28C71EA87E42" -H 'Content-type: application/json' -X POST -d '{"language": "python", "title": "test", "public": true, "files": [{"name": "main.py", "content": "print(42)"}]}' localhost:8091/snippets | jq .
