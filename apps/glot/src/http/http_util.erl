-module(http_util).

-export([
    decode_body/3,
    set_response_msg/2,
    add_cors_headers/2,
    add_allow_header/2,
    add_link_pagination_header/5,
    log_request/1,
    log_response/4
]).

-define(INVALID_JSON, <<"Invalid json">>).

log_request(Req) ->
    {Headers, _} = cowboy_req:headers(Req),
    {Method, _} = cowboy_req:method(Req),
    {Path, _} = cowboy_req:path(Req),
    {{Ip, Port}, _} = cowboy_req:peer(Req),
    log:http(#{
        peer => #{
            ip => list_to_binary(inet:ntoa(Ip)),
            port => Port
        },
        headers => Headers,
        method => Method,
        path => Path,
        type => request
    }),
    Req.

log_request_body(Body) ->
    log_body(Body, request_body).

log_response_body(Body) ->
    log_body(Body, response_body).

log_body(<<>>, _) ->
    ok;
log_body(Body, Type) ->
    log:http(#{
        body => Body,
        type => Type
    }).

log_response(Status, Headers, Body, Req) ->
    log:http(#{
        status => Status,
        headers => format_headers(Headers),
        type => response
    }),
    log_response_body(Body),
    Req.

format_headers([]) ->
    [];
format_headers([{K, V}|Rest]) when is_list(V) ->
    [{K, list_to_binary(V)}|format_headers(Rest)];
format_headers([{K, V}|Rest]) ->
    [{K, V}|format_headers(Rest)].

decode_body(F, Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    log_request_body(Body),
    case jsx:is_json(Body) of
        true ->
            Data = jsx:decode(Body),
            F(Data, Req2, State);
        false ->
            Payload = jsx:encode([{message, ?INVALID_JSON}]),
            {ok, Req3} = cowboy_req:reply(400, [], Payload, Req2),
            {halt, Req3, State}
    end.

add_cors_headers(Req, Methods) ->
    Headers = [
        {<<"access-control-allow-methods">>, join_methods(Methods)},
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-allow-headers">>, <<"Content-Type">>}
    ],
    set_headers(Headers, Req).

add_allow_header(Req, Methods) ->
    set_headers([{<<"allow">>, join_methods(Methods)}], Req).

join_methods(Methods) ->
    L = lists:foldr(fun(X, Acc) ->
        [X, <<", ">>|Acc]
    end, [], Methods),
    list_to_binary(lists:droplast(L)).

set_headers(Headers, Req) ->
    lists:foldl(fun({Name, Value}, R) ->
        cowboy_req:set_resp_header(Name, Value, R)
    end, Req, Headers).

set_response_msg(Msg, Req) ->
    Data = jsx:encode(#{message => Msg}),
    cowboy_req:set_resp_body(Data, Req).

proplist_to_qs(QsList) ->
    S = lists:foldr(fun({Key, Value}, Acc) ->
        [<<Key/binary, $=, Value/binary>>|Acc]
    end, [], QsList),
    util:join(S, $&).

add_link_pagination_header(Req, _, PageNo, PageCount, _) when PageNo > PageCount ->
    Req;
add_link_pagination_header(Req, BaseUrl, PageNo, PageCount, QsList) ->
    Value = link_pagination_header_value(BaseUrl, PageNo, PageCount, QsList),
    set_headers([{<<"link">>, Value}], Req).

link_pagination_header_value(BaseUrl, PageNo, PageCount, QsList) ->
    QsTail = proplists:delete(<<"page">>, QsList),
    HeaderVals = link_header_vals(BaseUrl, PageNo, PageCount, QsTail),
    util:join(HeaderVals, <<", ">>).

link_header_vals(BaseUrl, 1, 1, QsList) ->
    link_last_vals(BaseUrl, 1, QsList);
link_header_vals(BaseUrl, 1, PageCount, QsList) ->
    link_next_last_vals(BaseUrl, 1, PageCount, QsList);
link_header_vals(BaseUrl, PageNo, PageCount, QsList) when PageNo == PageCount ->
    link_prev_first_vals(BaseUrl, PageNo, QsList);
link_header_vals(BaseUrl, PageNo, PageCount, QsList) ->
    NextLast = link_next_last_vals(BaseUrl, PageNo, PageCount, QsList),
    PrevFirst = link_prev_first_vals(BaseUrl, PageNo, QsList),
    NextLast ++ PrevFirst.

link_last_vals(BaseUrl, PageCount, QsList) ->
    [build_link_header(BaseUrl, PageCount, "last", QsList)].

link_next_last_vals(BaseUrl, PageNo, PageCount, QsList) ->
    [
        build_link_header(BaseUrl, PageNo + 1, "next", QsList)|
        link_last_vals(BaseUrl, PageCount, QsList)
    ].

link_prev_first_vals(BaseUrl, PageNo, QsList) ->
    [
        build_link_header(BaseUrl, PageNo - 1, "prev", QsList),
        build_link_header(BaseUrl, 1, "first", QsList)
    ].

build_link_header(BaseUrl, PageNo, Rel, QsList) ->
    Page = integer_to_binary(PageNo),
    Qs = [$?, proplist_to_qs([{<<"page">>, Page}|QsList])],
    [$<, BaseUrl, Qs, $>, "; rel=", $", Rel, $"].
