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
    list_public/1,
    list_public_by_owner/2,

    count_by_owner/1,
    count_public/0,
    count_public_by_owner/1,

    get/1,
    save/1,
    delete/1
]).

-record(state, {
    server,
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

count_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  emit(doc.owner, 1);"
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

count_public_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(null, 1);"
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

count_public_by_owner_map_func() ->
    <<
    "function(doc) {"
    "  if (doc.public) {"
    "    emit(doc.owner, 1);"
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
            {<<"count_by_owner">>, [
                {<<"map">>, count_by_owner_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"list_public">>, [
                {<<"map">>, list_public_map_func()}
            ]},
            {<<"count_public">>, [
                {<<"map">>, count_public_map_func()},
                {<<"reduce">>, <<"_count">>}
            ]},
            {<<"list_public_by_owner">>, [
                {<<"map">>, list_public_by_owner_map_func()}
            ]},
            {<<"count_public_by_owner">>, [
                {<<"map">>, count_public_by_owner_map_func()},
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
    {ok, #state{server=Server, db=Db}}.

stop() ->
    gen_server:call(?MODULE, stop).

handle_call({list_by_owner, Owner, {Limit, Skip}}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_by_owner"}, [
        {key, Owner}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_rows(Rows), State};
handle_call({list_public, {Limit, Skip}}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public"}, [
        {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_rows(Rows), State};
handle_call({list_public_by_owner, Owner, {Limit, Skip}}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "list_public_by_owner"}, [
        {key, Owner}, {limit, Limit}, {skip, Skip}, descending
    ]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_rows(Rows), State};
handle_call({count_by_owner, Owner}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_by_owner"}, [{key, Owner}, group]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_count(format_rows(Rows)), State};
handle_call({count_public}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public"}, []),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_count(format_rows(Rows)), State};
handle_call({count_public_by_owner, Owner}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"snippets", "count_public_by_owner"}, [{key, Owner}, group]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_count(format_rows(Rows)), State};
handle_call({get, Id}, _From, State=#state{db=Db}) ->
    Res = case couchbeam:open_doc(Db, Id) of
        {ok, Data} -> {ok, util:jiffy_to_jsx_terms(Data)};
        Error -> Error
    end,
    {reply, Res, State};
handle_call({save, Snippet}, _From, State=#state{db=Db}) ->
    {ok, Res} = couchbeam:save_doc(Db, util:jsx_to_jiffy_terms(Snippet)),
    {reply, util:jiffy_to_jsx_terms(Res), State};
handle_call({delete, Snippet}, _From, State=#state{db=Db}) ->
    couchbeam:delete_doc(Db, util:jsx_to_jiffy_terms(Snippet)),
    {reply, ok, State};
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

list_by_owner(Owner, Pagination) ->
    gen_server:call(?MODULE, {list_by_owner, Owner, Pagination}).

list_public(Pagination) ->
    gen_server:call(?MODULE, {list_public, Pagination}).

list_public_by_owner(Owner, Pagination) ->
    gen_server:call(?MODULE, {list_public_by_owner, Owner, Pagination}).

count_by_owner(Owner) ->
    gen_server:call(?MODULE, {count_by_owner, Owner}).

count_public() ->
    gen_server:call(?MODULE, {count_public}).

count_public_by_owner(Owner) ->
    gen_server:call(?MODULE, {count_public_by_owner, Owner}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

save(Snippet) ->
    gen_server:call(?MODULE, {save, Snippet}).

delete(Snippet) ->
    gen_server:call(?MODULE, {delete, Snippet}).

format_rows(Rows) ->
    [proplists:get_value(<<"value">>, X) || X <- Rows].

format_count([]) -> 0;
format_count([N]) -> N.
