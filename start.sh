#!/bin/sh
export API_ENVIRONMENT="development"
export API_HTTP_LISTEN_IP="0.0.0.0"
export API_HTTP_LISTEN_PORT="8091"
export DB_URL="http://10.0.0.182:5984"
export DB_USER="glot"
export DB_PASSWORD="bg5zU7LA3mWbeQ"
export LOG_PATH="/tmp/glot-snippets/log/"
export BASE_URL="http://localhost:8091"
export ADMIN_TOKEN="clumeterin"

cd `dirname $0`
exec erl -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -s glot
