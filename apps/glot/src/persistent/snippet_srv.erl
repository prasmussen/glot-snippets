-module(snippet_srv).
-behaviour(gen_server).
-export([
    start_link/0,
    stop/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2,

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
    delete/1
]).

-record(state, {
    db    
}).

setup(Server) ->
    {ok, Db} = couchbeam:open_or_create_db(Server, "snippets"),
    create_or_update_design(Db),
    Db.

create_or_update_design(Db) ->
    Design = design_doc(),
    Id = couchbeam_doc:get_id(Design),
    NewDesign = case couchbeam:open_doc(Db, Id) of
        {ok, Doc} ->
            Rev = couchbeam_doc:get_rev(Doc),
            couchbeam_doc:set_value(<<"_rev">>, Rev, Design);
        {error, not_found} -> Design
    end,
    couchbeam:save_doc(Db, NewDesign).

list_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  emit(doc.owner, {"
    "    id: doc._id,"
    "    created: doc.created,"
    "    modified: doc.modified,"
    "    language: doc.language,"
    "    title: doc.title,"
    "    public: doc.public,"
    "    owner: doc.owner,"
    "    files_hash: doc.files_hash,"
    "  });"
    "}"
    >>.

list_by_owner_by_language_map_func() ->
    <<
    "function(doc) {"
    "  emit([doc.owner, doc.language], {"
    "    id: doc._id,"
    "    created: doc.created,"
    "    modified: doc.modified,"
    "    language: doc.language,"
    "    title: doc.title,"
    "    public: doc.public,"
    "    owner: doc.owner,"
    "    files_hash: doc.files_hash,"
    "  });"
    "}"
    >>.

list_public_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc._id, {"
    "      id: doc._id,"
    "      created: doc.created,"
    "      modified: doc.modified,"
    "      language: doc.language,"
    "      title: doc.title,"
    "      public: doc.public,"
    "      owner: doc.owner,"
    "      files_hash: doc.files_hash,"
    "    });"
    "  }"
    "}"
    >>.

list_public_by_language_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc.language, {"
    "      id: doc._id,"
    "      created: doc.created,"
    "      modified: doc.modified,"
    "      language: doc.language,"
    "      title: doc.title,"
    "      public: doc.public,"
    "      owner: doc.owner,"
    "      files_hash: doc.files_hash,"
    "    });"
    "  }"
    "}"
    >>.

list_public_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc.owner, {"
    "      id: doc._id,"
    "      created: doc.created,"
    "      modified: doc.modified,"
    "      language: doc.language,"
    "      title: doc.title,"
    "      public: doc.public,"
    "      owner: doc.owner,"
    "      files_hash: doc.files_hash,"
    "    });"
    "  }"
    "}"
    >>.

list_public_by_owner_by_language_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit([doc.owner, doc.language], {"
    "      id: doc._id,"
    "      created: doc.created,"
    "      modified: doc.modified,"
    "      language: doc.language,"
    "      title: doc.title,"
    "      public: doc.public,"
    "      owner: doc.owner,"
    "      files_hash: doc.files_hash,"
    "    });"
    "  }"
    "}"
    >>.

count_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  emit(doc.owner, 1);"
    "}"
    >>.

count_by_owner_by_language_map_func() ->
    <<
    "function(doc) {"
    "  emit([doc.owner, doc.language], 1);"
    "}"
    >>.

count_public_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(null, 1);"
    "  }"
    "}"
    >>.

count_public_by_language_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc.language, 1);"
    "  }"
    "}"
    >>.

count_public_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc.owner, 1);"
    "  }"
    "}"
    >>.

count_public_by_owner_by_language_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit([doc.owner, doc.language], 1);"
    "  }"
    "}"
    >>.

design_doc() ->
    util:jsx_to_jiffy_terms([
        {<<"_id">>, <<"_design/snippets">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, [
            {<<"list_by_owner">>, [
                {<<"map">>, list_by_owner_map_func()}
            ]},
            {<<"list_by_owner_by_language">>, [
                {<<"map">>, list_by_owner_by_language_map_func()}
            ]},
            {<<"list_public">>, [
                {<<"map">>, list_public_map_func()}
            ]},
            {<<"list_public_by_language">>, [
                {<<"map">>, list_public_by_language_map_func()}
            ]},
            {<<"list_public_by_owner">>, [
                {<<"map">>, list_public_by_owner_map_func()}
            ]},
            {<<"list_public_by_owner_by_language">>, [
                {<<"map">>, list_public_by_owner_by_language_map_func()}
            ]},
            {<<"count_by_owner">>, [
                {<<"map">>, count_by_owner_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"count_by_owner_by_language">>, [
                {<<"map">>, count_by_owner_by_language_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"count_public">>, [
                {<<"map">>, count_public_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"count_public_by_language">>, [
                {<<"map">>, count_public_by_language_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"count_public_by_owner">>, [
                {<<"map">>, count_public_by_owner_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"count_public_by_owner_by_language">>, [
                {<<"map">>, count_public_by_owner_by_language_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]}
        ]}
    ]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Url = config:db_url(),
    Server = couchbeam:server_connection(Url, [
        {basic_auth, {config:db_user(), config:db_password()}}
    ]),
    Db = setup(Server),
    {ok, #state{db=Db}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({get_db}, _From, State=#state{db=Db}) ->
    {reply, Db, State};
handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsc, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    Reason.

list_by_owner(Owner, {Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_by_owner"}, [
        {key, Owner}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

list_by_owner_by_language(Owner, Language, {Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_by_owner_by_language"}, [
        {key, [Owner, Language]}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

list_public({Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public"}, [
        {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

list_public_by_language(Language, {Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public_by_language"}, [
        {key, Language}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

list_public_by_owner(Owner, {Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public_by_owner"}, [
        {key, Owner}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

list_public_by_owner_by_language(Owner, Language, {Limit, Skip}) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public_by_owner_by_language"}, [
        {key, [Owner, Language]}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_rows(Rows).

count_by_owner(Owner) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_by_owner"}, [{key, Owner}, group]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

count_by_owner_by_language(Owner, Language) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_by_owner_by_language"}, [
        {key, [Owner, Language]}, group]
    ),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

count_public() ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public"}, []),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

count_public_by_language(Language) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public_by_language"}, [
        {key, Language}
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

count_public_by_owner(Owner) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public_by_owner"}, [{key, Owner}, group]),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

count_public_by_owner_by_language(Owner, Language) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public_by_owner_by_language"}, [
        {key, [Owner, Language]}, group]
    ),
    Rows = util:jiffy_to_jsx_terms(Data),
    format_count(format_rows(Rows)).

get(Id) ->
    Db = gen_server:call(?MODULE, {get_db}),
    case couchbeam:open_doc(Db, Id) of
        {ok, Data} -> {ok, util:jiffy_to_jsx_terms(Data)};
        Error -> Error
    end.

save(Snippet) ->
    Db = gen_server:call(?MODULE, {get_db}),
    {ok, Res} = couchbeam:save_doc(Db, util:jsx_to_jiffy_terms(Snippet)),
    util:jiffy_to_jsx_terms(Res).

delete(Snippet) ->
    Db = gen_server:call(?MODULE, {get_db}),
    couchbeam:delete_doc(Db, util:jsx_to_jiffy_terms(Snippet)),
    ok.

format_rows(Rows) ->
    [proplists:get_value(<<"value">>, X) || X <- Rows].

format_count([]) -> 0;
format_count([N]) -> N.
