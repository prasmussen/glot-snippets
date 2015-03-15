-module(snippet).
-export([
    list_by_owner/1,
    get/1,
    save/1,
    update/3,
    delete/1,

    is_public/1,
    is_owner/2
]).

is_public(Snippet) ->
    proplists:get_value(<<"public">>, Snippet).

is_owner(Snippet, Owner) ->
    proplists:get_value(<<"owner">>, Snippet) =:= Owner.

identifier() ->
    list_to_binary(util:base10_to_36(util:microseconds_since_epoch())).

id_to_iso8601(Id) ->
    Ts = util:microseconds_to_timestamp(util:base36_to_10(binary_to_list(Id))),
    iso8601:format(Ts).

list_by_owner(Owner) ->
    snippet_srv:list_by_owner(Owner).

get(Id) ->
    snippet_srv:get(Id).

save(Snippet) ->
    NewSnippet = prepare_save(Snippet),
    snippet_srv:save(NewSnippet).

update(Id, Rev, Snippet) ->
    NewSnippet = prepare_update(Id, Rev, Snippet),
    snippet_srv:save(NewSnippet).

delete(Snippet) ->
    snippet_srv:delete(Snippet).

prepare_save(Data) ->
    Id = identifier(),
    [
        {<<"_id">>, Id},
        {<<"created">>, id_to_iso8601(Id)},
        {<<"modified">>, id_to_iso8601(Id)}
        |Data
    ].

prepare_update(Id, Rev, Data) ->
    Now = iso8601:format(now()),
    [
        {<<"_id">>, Id},
        {<<"_rev">>, Rev},
        {<<"created">>, id_to_iso8601(Id)},
        {<<"modified">>, Now}
        |Data
    ].
