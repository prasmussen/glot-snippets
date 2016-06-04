-module(util).

-export([
    sha1/1,
    ceil/1,
    join/2,
    get_bool_value/3,
    get_bin_value/3,
    pid_to_binary/1,
    microseconds_since_epoch/0,
    microseconds_to_timestamp/1,
    base10_to_36/1,
    base36_to_10/1,
    jiffy_to_jsx_terms/1,
    jsx_to_jiffy_terms/1
]).

sha1(Data) ->
    hexstring(crypto:hash(sha, Data)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:224/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~56.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:384/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~96.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    iolist_to_binary(io_lib:format("~128.16.0b", [X])).

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

microseconds_since_epoch() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    (Megasecs * 1000000 + Secs) * 1000000 + Microsecs.

microseconds_to_timestamp(N) ->
    Megasecs = N div 1000000 div 1000000,
    Secs = N div 1000000 rem 1000000,
    Microsecs = N rem 1000000,
    {Megasecs, Secs, Microsecs}.

base10_to_36(N) ->
    string:to_lower(integer_to_list(N, 36)).

base36_to_10(S) ->
    list_to_integer(S, 36).

jiffy_to_jsx_terms({[]}) ->
    [{}];
jiffy_to_jsx_terms({PropList}) ->
    jiffy_to_jsx_terms(PropList);
jiffy_to_jsx_terms([{_, _}|_] = PropList) ->
    [ {Key, jiffy_to_jsx_terms(Value)} || {Key, Value} <- PropList ];
jiffy_to_jsx_terms(List) when is_list(List) ->
    [ jiffy_to_jsx_terms(Term) || Term <- List ];
jiffy_to_jsx_terms(true) ->
    true;
jiffy_to_jsx_terms(false) ->
    false;
jiffy_to_jsx_terms(null) ->
    null;
jiffy_to_jsx_terms(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
jiffy_to_jsx_terms(Term) when is_integer(Term); is_float(Term); is_binary(Term) ->
    Term.

jsx_to_jiffy_terms([{}]) ->
    {[]};
jsx_to_jiffy_terms([{_Key, _Value} | _Rest] = PropList) ->
    {[ {Key, jsx_to_jiffy_terms(Value)} || {Key, Value} <- PropList ]};
jsx_to_jiffy_terms(List) when is_list(List) ->
    [ jsx_to_jiffy_terms(Term) || Term <- List];
jsx_to_jiffy_terms(Term) ->
    Term.

ceil(X) when X < 0 -> trunc(X);
ceil(X) when X > trunc(X) -> trunc(X) + 1;
ceil(X) -> X.

join(Parts, Sep) ->
    L = lists:foldr(fun(X, Acc) ->
        [X, Sep|Acc]
    end, [], Parts),
    iolist_to_binary(lists:droplast(L)).

get_bool_value(Key, Proplist, Default) ->
    case proplists:get_value(Key, Proplist) of
        true -> true;
        _ -> Default
    end.

get_bin_value(Key, Proplist, Default) ->
    Value = proplists:get_value(Key, Proplist, Default),
    format_bin_value(Value, Default).

format_bin_value(<<"">>, Default) -> Default;
format_bin_value(Value, _) when is_binary(Value) -> Value;
format_bin_value(_, Default) -> Default.
