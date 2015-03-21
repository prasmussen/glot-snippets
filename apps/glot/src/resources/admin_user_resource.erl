-module(admin_user_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    is_authorized/2,
    resource_exists/2,
    get/2,
    accept_put/2,
    delete_resource/2
]).

-record(state, {
    id,
    user
}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state{}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_accepted(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, accept_put}
    ],
    {Handlers, Req, State}.

content_types_provided(Req, State) ->
    Handlers = [
        {{<<"application">>, <<"json">>, '*'}, get}
    ],
    {Handlers, Req, State}.

is_authorized(Req, State) ->
    http_auth:authorize_admin(Req, State).

resource_exists(Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    case users:get(Id) of
        {ok, User} ->
            {true, Req, #state{id=Id, user=User}};
        {error, not_found} ->
            {false, Req, State}
    end.

get(Req, State=#state{user=User}) ->
    {prepare_response(User), Req, State}.

accept_put(Req, State) ->
    http_util:decode_body(fun update_user/3, Req, State).

update_user(Data, Req, State=#state{id=Id, user=User}) ->
    Rev = proplists:get_value(<<"_rev">>, User),
    NewUser = users:update(Id, Rev, normalize(Data)),
    Req2 = cowboy_req:set_resp_body(prepare_response(NewUser), Req),
    {true, Req2, State}.

delete_resource(Req, State=#state{id=Id, user=User}) ->
    Rev = proplists:get_value(<<"_rev">>, User),
    users:delete(Id, Rev),
    {true, Req, State}.

normalize(Data) ->
    [
        {<<"token">>, proplists:get_value(<<"token">>, Data, <<>>)}
    ].

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
