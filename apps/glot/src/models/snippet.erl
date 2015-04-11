-module(snippet).
-export([
    list_by_owner/2,
    list_by_owner_by_language/3,
    list_public/1,
    list_public_by_language/2,
    list_public_by_owner/2,
    list_public_by_owner_by_language/3,

    count_by_owner/1,
    count_by_owner_by_language/2,
    count_public/0,
    count_public_by_language/1,
    count_public_by_owner/1,
    count_public_by_owner_by_language/2,

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

list_by_owner(Owner, Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_by_owner(Owner, DbPagination).

list_by_owner_by_language(Owner, Language, Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_by_owner_by_language(Owner, Language, DbPagination).

list_public(Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_public(DbPagination).

list_public_by_language(Language, Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_public_by_language(Language, DbPagination).

list_public_by_owner(Owner, Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_public_by_owner(Owner, DbPagination).

list_public_by_owner_by_language(Owner, Language, Pagination) ->
    DbPagination = to_db_pagination(Pagination),
    snippet_srv:list_public_by_owner_by_language(Owner, Language, DbPagination).

count_by_owner(Owner) ->
    snippet_srv:count_by_owner(Owner).

count_by_owner_by_language(Owner, Language) ->
    snippet_srv:count_by_owner_by_language(Owner, Language).

count_public() ->
    snippet_srv:count_public().

count_public_by_language(Language) ->
    snippet_srv:count_public_by_language(Language).

count_public_by_owner(Owner) ->
    snippet_srv:count_public_by_owner(Owner).

count_public_by_owner_by_language(Owner, Language) ->
    snippet_srv:count_public_by_owner_by_language(Owner, Language).

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

to_db_pagination({PageNo, PerPage}) ->
    {PerPage, (PageNo - 1) * PerPage}.

hash_files(Snippet) ->
    Files = proplists:get_value(<<"files">>, Snippet),
    util:sha1(jsx:encode(Files)).

prepare_save(Data) ->
    Id = identifier(),
    [
        {<<"_id">>, Id},
        {<<"created">>, id_to_iso8601(Id)},
        {<<"modified">>, id_to_iso8601(Id)},
        {<<"files_hash">>, hash_files(Data)}
        |Data
    ].

prepare_update(Id, Rev, Data) ->
    Now = iso8601:format(now()),
    [
        {<<"_id">>, Id},
        {<<"_rev">>, Rev},
        {<<"created">>, id_to_iso8601(Id)},
        {<<"modified">>, Now},
        {<<"files_hash">>, hash_files(Data)}
        |Data
    ].
