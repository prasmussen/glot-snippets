-module(user_srv).
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

    get_by_token/1,
    get/1,
    list/0,
    save/1,
    delete/1
]).

-record(state, {
    server,
    db
}).

setup(Server) ->
    {ok, Db} = couchbeam:open_or_create_db(Server, "users"),
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

get_by_token_map_func() ->
    <<
    "function(doc) {"
    "  emit(doc.token, {"
    "    id: doc._id,"
    "    token: doc.token,"
    "  });"
    "}"
    >>.

list_map_func() ->
    <<
    "function(doc) {"
    "  emit(doc._id, doc);"
    "}"
    >>.

design_doc() ->
    util:jsx_to_jiffy_terms([
        {<<"_id">>, <<"_design/users">>},
        {<<"language">>, <<"javascript">>},
        {<<"views">>, [
            {<<"get_by_token">>, [
                {<<"map">>, get_by_token_map_func()}
            ]},
            {<<"list">>, [
                {<<"map">>, list_map_func()}
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

handle_call({get_by_token, Token}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"users", "get_by_token"}, [{key, Token}]),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_rows(Rows), State};
handle_call({list}, _From, State=#state{db=Db}) ->
    {ok, Data} = couchbeam_view:fetch(Db, {"users", "list"}, []),
    Rows = util:jiffy_to_jsx_terms(Data),
    {reply, format_rows(Rows), State};
handle_call({get, Id}, _From, State=#state{db=Db}) ->
    Res = case couchbeam:open_doc(Db, Id) of
        {ok, Data} -> {ok, util:jiffy_to_jsx_terms(Data)};
        Error -> Error
    end,
    {reply, Res, State};
handle_call({save, User}, _From, State=#state{db=Db}) ->
    {ok, Res} = couchbeam:save_doc(Db, util:jsx_to_jiffy_terms(User)),
    {reply, util:jiffy_to_jsx_terms(Res), State};
handle_call({delete, User}, _From, State=#state{db=Db}) ->
    couchbeam:delete_doc(Db, util:jsx_to_jiffy_terms(User)),
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

get_by_token(Token) ->
    gen_server:call(?MODULE, {get_by_token, Token}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

list() ->
    gen_server:call(?MODULE, {list}).

save(User) ->
    gen_server:call(?MODULE, {save, User}).

delete(User) ->
    gen_server:call(?MODULE, {delete, User}).

format_rows(Rows) ->
    [proplists:get_value(<<"value">>, X) || X <- Rows].
