-module(glot_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case config:environment() of
        development ->
            application:start(sync);
        _ -> noop
    end,
    start_http_server(),
    glot_sup:start_link().

stop(_State) ->
    ok.

start_http_server() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", root_resource, []},
            {"/admin", admin_root_resource, []},
            {"/admin/users", admin_users_resource, []},
            {"/admin/users/:id", admin_user_resource, []},
            {"/snippets", snippets_resource, []},
            {"/snippets/:id", snippet_resource, []},
            {"/snippets/:id/star", todo, []}
        ]}
    ]),

    {ok, _Pid} = cowboy:start_http(http, 100,
        [
            {ip, config:http_listen_ip()},
            {port, config:http_listen_port()}
        ],
        [
            {env, [{dispatch, Dispatch}]},
            {onrequest, fun http_util:log_request/1},
            {onresponse, fun http_util:log_response/4}
        ]
    ).
