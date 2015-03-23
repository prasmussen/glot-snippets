-module(admin_users_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    list/2,
    accept_post/2
]).


init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, []}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, accept_post}
    ],
    {Handlers, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, list}
    ],
    {Handlers, Req, State}.

is_authorized(Req, State) ->
    http_auth:authorize_admin(Req, State).

list(Req, State) ->
    Users = users:list(),
    {prepare_list_response(Users), Req, State}.

accept_post(Req, State) ->
    http_util:decode_body(fun save/3, Req, State).

save(Data, Req, State) ->
    User = users:save(normalize(Data)),
    Req2 = cowboy_req:set_resp_body(prepare_response(User), Req),
    {true, Req2, State}.

normalize(Data) ->
    [
        {<<"token">>, proplists:get_value(<<"token">>, Data, <<>>)}
    ].

prepare_list_response(Users) ->
    jsx:encode([format_user(X) || X <- Users]).

prepare_response(User) ->
    jsx:encode(format_user(User)).

format_user(User) ->
    Id = proplists:get_value(<<"_id">>, User),
    Mutators = [
        fun(X) -> proplists:delete(<<"_id">>, X) end,
        fun(X) -> proplists:delete(<<"_rev">>, X) end,
        fun(X) -> [{<<"id">>, Id}, {<<"url">>, get_url(Id)}|X] end
    ],
    lists:foldl(fun(F, Acc) -> F(Acc) end, User, Mutators).

get_url(Id) ->
    BaseUrl = config:base_url(),
    <<BaseUrl/binary, "/admin/users/", Id/binary>>.
