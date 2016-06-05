-module(users).
-export([
    get/1,
    get_by_token/1,
    list/0,
    save/1,
    update/3,
    delete/2
]).

identifier() ->
    Uuid = uuid:uuid_to_string(uuid:get_v4()),
    list_to_binary(Uuid).

get_by_token(Token) ->
    case user_srv:get_by_token(Token) of
        [] -> {error, not_found};
        [Doc] -> {ok, Doc}
    end.

get(Id) ->
    user_srv:get(Id).

list() ->
    user_srv:list().

save(User) ->
    Doc = prepare_save(User),
    user_srv:save(Doc).

update(Id, OldUser, User) ->
    Doc = prepare_update(Id, OldUser, User),
    user_srv:save(Doc).

delete(Id, Rev) ->
    Doc = prepare_delete(Id, Rev),
    user_srv:delete(Doc).

prepare_save(Data) ->
    Now = iso8601:format(os:timestamp()),
    [
        {<<"_id">>, identifier()},
        {<<"created">>, Now},
        {<<"modified">>, Now}
        |Data
    ].

prepare_update(Id, OldUser, Data) ->
    Now = iso8601:format(os:timestamp()),
    [
        {<<"_id">>, Id},
        {<<"_rev">>, proplists:get_value(<<"_rev">>, OldUser)},
        {<<"created">>, proplists:get_value(<<"created">>, OldUser)},
        {<<"modified">>, Now}
        |Data
    ].

prepare_delete(Id, Rev) ->
    [
        {<<"_id">>, Id},
        {<<"_rev">>, Rev}
    ].
