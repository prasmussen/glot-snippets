-module(snippets_resource).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
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

list(Req, State) ->
    Owner = <<"136eeed948805a00846ad95939037ff8">>,
    Snippets = snippet:list_by_owner(Owner),
    {prepare_list_response(Snippets), Req, State}.

accept_post(Req, State) ->
    http_util:decode_body(fun save_snippet/3, Req, State).

save_snippet(Data, Req, State) ->
    Owner = <<"136eeed948805a00846ad95939037ff8">>,
    NormalizeData = [{<<"owner">>, Owner}|normalize(Data)],
    Snippet = snippet:save(NormalizeData),
    Req2 = cowboy_req:set_resp_body(prepare_response(Snippet), Req),
    {true, Req2, State}.

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

prepare_list_response(Snippets) ->
    NewSnippets = lists:map(fun(X) ->
        Id = proplists:get_value(<<"id">>, X),
        [{<<"url">>, get_url(Id)}|X]
    end, Snippets),
    jsx:encode(NewSnippets).

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
