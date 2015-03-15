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

-record(state, {
    id,
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
    {Id, _} = cowboy_req:binding(id, Req),
    {Method, _} = cowboy_req:method(Req),
    Owner = <<"136eeed948805a00846ad95939037ff8">>,
    case snippet:get(Id) of
        {ok, Snippet} ->
            Forbidden = not is_allowed(Method, Owner, Snippet),
            {Forbidden, Req, State#state{id=Id, snippet=Snippet}};
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

update_snippet(Data, Req, State=#state{id=Id, snippet=Snippet}) ->
    Owner = <<"136eeed948805a00846ad95939037ff8">>,
    Rev = proplists:get_value(<<"_rev">>, Snippet),
    NormalizeData = [{<<"owner">>, Owner}|normalize(Data)],
    NewSnippet = snippet:update(Id, Rev, NormalizeData),
    Req2 = cowboy_req:set_resp_body(prepare_response(NewSnippet), Req),
    {true, Req2, State}.

is_allowed(<<"GET">>, Owner, Snippet) ->
    snippet:is_owner(Snippet, Owner) orelse snippet:is_public(Snippet);
is_allowed(<<"PUT">>, Owner, Snippet) ->
    snippet:is_owner(Snippet, Owner);
is_allowed(<<"DELETE">>, Owner, Snippet) ->
    snippet:is_owner(Snippet, Owner).

% TODO: Is duplicated
prepare_response(Snippet) ->
    Id = proplists:get_value(<<"_id">>, Snippet),
    Mutators = [
        fun(X) -> proplists:delete(<<"_id">>, X) end,
        fun(X) -> proplists:delete(<<"_rev">>, X) end,
        fun(X) -> proplists:delete(<<"owner">>, X) end,
        fun(X) -> [{<<"id">>, Id}, {<<"url">>, get_url(Id)}|X] end
    ],
    NewSnippet = lists:foldl(fun(F, Acc) -> F(Acc) end, Snippet, Mutators),
    jsx:encode(NewSnippet).

get_url(Id) ->
    BaseUrl = config:base_url(),
    <<BaseUrl/binary, "/snippets/", Id/binary>>.

normalize(Data) ->
    Files = [normalize_file(X) || X <- proplists:get_value(<<"files">>, Data, [])],
    [
        {<<"language">>, proplists:get_value(<<"language">>, Data, <<>>)},
        {<<"title">>, proplists:get_value(<<"title">>, Data, <<"untitled">>)},
        {<<"public">>, proplists:get_value(<<"public">>, Data, false)},
        {<<"files">>, Files}
    ].

normalize_file(Data) ->
    [
        {<<"name">>, proplists:get_value(<<"name">>, Data, <<"untitled">>)},
        {<<"content">>, proplists:get_value(<<"content">>, Data, <<>>)}
    ].
