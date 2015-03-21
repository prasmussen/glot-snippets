-module(snippet_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    forbidden/2,
    resource_exists/2,
    delete_resource/2,
    get/2,
    accept_put/2
]).

-define(WRONG_TOKEN, <<"Wrong auth token">>).

-record(state, {
    id,
    user_id,
    snippet
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

forbidden(Req, State) ->
    case lookup_user_id(Req) of
        {ok, UserId} ->
            forbidden(UserId, Req, State);
        {error, not_found} ->
            Data = jsx:encode(#{message => ?WRONG_TOKEN}),
            Req2 = cowboy_req:set_resp_body(Data, Req),
            {true, Req2, State}
    end.

forbidden(UserId, Req, State) ->
    {Id, _} = cowboy_req:binding(id, Req),
    {Method, _} = cowboy_req:method(Req),
    case snippet:get(Id) of
        {ok, Snippet} ->
            Forbidden = not is_allowed(Method, UserId, Snippet),
            NewState = State#state{id=Id, user_id=UserId, snippet=Snippet},
            {Forbidden, Req, NewState};
        {error, not_found} ->
            {false, Req, State}
    end.

resource_exists(Req, State=#state{id=undefined}) ->
    {ok, Req2} = cowboy_req:reply(404, Req),
    {halt, Req2, State};
resource_exists(Req, State) ->
    {true, Req, State}.

delete_resource(Req, State=#state{snippet=Snippet}) ->
    snippet:delete(Snippet),
    {true, Req, State}.

get(Req, State=#state{snippet=Snippet}) ->
    {prepare_response(Snippet), Req, State}.

accept_put(Req, State) ->
    http_util:decode_body(fun update_snippet/3, Req, State).

update_snippet(Data, Req, State=#state{id=Id, user_id=UserId, snippet=Snippet}) ->
    Rev = proplists:get_value(<<"_rev">>, Snippet),
    NewSnippet = snippet:update(Id, Rev, normalize(UserId, Data)),
    Req2 = cowboy_req:set_resp_body(prepare_response(NewSnippet), Req),
    {true, Req2, State}.

lookup_user_id(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {ok, {<<"token">>, Token}, _} -> get_user_id(Token);
        _ -> {ok, <<"anonymous">>}
    end.

get_user_id(Token) ->
    case users:get_by_token(Token) of
        {ok, User} -> {ok, proplists:get_value(<<"id">>, User)};
        Error -> Error
    end.

is_allowed(<<"GET">>, UserId, Snippet) ->
    snippet:is_owner(Snippet, UserId) orelse snippet:is_public(Snippet);
is_allowed(<<"PUT">>, <<"anonymous">>, _) ->
    false;
is_allowed(<<"PUT">>, UserId, Snippet) ->
    snippet:is_owner(Snippet, UserId);
is_allowed(<<"DELETE">>, <<"anonymous">>, _) ->
    false;
is_allowed(<<"DELETE">>, UserId, Snippet) ->
    snippet:is_owner(Snippet, UserId).

% TODO: Is duplicated
prepare_response(Snippet) ->
    Id = proplists:get_value(<<"_id">>, Snippet),
    Mutators = [
        fun(X) -> proplists:delete(<<"_id">>, X) end,
        fun(X) -> proplists:delete(<<"_rev">>, X) end,
        fun(X) -> [{<<"id">>, Id}, {<<"url">>, get_url(Id)}|X] end
    ],
    NewSnippet = lists:foldl(fun(F, Acc) -> F(Acc) end, Snippet, Mutators),
    jsx:encode(NewSnippet).

get_url(Id) ->
    BaseUrl = config:base_url(),
    <<BaseUrl/binary, "/snippets/", Id/binary>>.

normalize(UserId, Data) ->
    Files = [normalize_file(X) || X <- proplists:get_value(<<"files">>, Data, [])],
    [
        {<<"language">>, proplists:get_value(<<"language">>, Data, <<>>)},
        {<<"title">>, proplists:get_value(<<"title">>, Data, <<"untitled">>)},
        {<<"public">>, proplists:get_value(<<"public">>, Data, false)},
        {<<"owner">>, UserId},
        {<<"files">>, Files}
    ].

normalize_file(Data) ->
    [
        {<<"name">>, proplists:get_value(<<"name">>, Data, <<"untitled">>)},
        {<<"content">>, proplists:get_value(<<"content">>, Data, <<>>)}
    ].
