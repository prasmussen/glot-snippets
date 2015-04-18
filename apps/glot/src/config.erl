-module(config).

-export([
    environment/0,
    glot_version/0,
    glot_description/0,
    http_listen_ip/0,
    http_listen_port/0,
    http_log_path/0,
    event_log_path/0,
    base_url/0,
    db_url/0,
    db_user/0,
    db_password/0,
    admin_token/0
]).

environment() ->
    list_to_atom(os:getenv("API_ENVIRONMENT")).

glot_version() ->
    {ok, Vsn} = application:get_key(glot, vsn),
    list_to_binary(Vsn).

glot_description() ->
    {ok, Desc} = application:get_key(glot, description),
    list_to_binary(Desc).

http_listen_ip() ->
    {ok, Addr} = inet:parse_address(os:getenv("API_HTTP_LISTEN_IP")),
    Addr.

http_listen_port() ->
    list_to_integer(os:getenv("API_HTTP_LISTEN_PORT")).

log_path() ->
    Path = os:getenv("LOG_PATH"),
    filelib:ensure_dir(Path),
    Path.

http_log_path() ->
    filename:join(log_path(), "http.log").

event_log_path() ->
    filename:join(log_path(), "event.log").

base_url() ->
    list_to_binary(os:getenv("BASE_URL")).

db_url() ->
    os:getenv("DB_URL").

db_user() ->
    os:getenv("DB_USER").

db_password() ->
    os:getenv("DB_PASSWORD").

admin_token() ->
    list_to_binary(os:getenv("ADMIN_TOKEN")).
