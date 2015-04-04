-module(http_util).

-export([
    decode_body/3,
    set_response_msg/2,
    add_cors_headers/2,
    add_allow_header/2,
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
