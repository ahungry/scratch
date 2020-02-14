-module(json).
-compile(export_all).

%% Try to write a JSON parser in erlang (few days xp with it).
%% Read the strings, make something to represent them.
parse("{" ++ Rest) -> {open_brace, Rest};
parse("}" ++ Rest) -> {close_brace, Rest};
parse(" " ++ Rest) -> {ws, Rest};
parse("'" ++ Rest) -> {quote, Rest};
parse(":" ++ Rest) -> {colon, Rest};
parse("")          -> eot;
parse([H|T])       -> {{any, H}, T}.

parser(S) -> parser(parse(S), []).

parser(eot, Acc) -> lists:reverse(Acc);
parser({Sym, Rest}, Acc) ->
    parser(parse(Rest), [Sym|Acc]).

is_any({any, _}) -> true;
is_any(_) -> false.

%% Iterate on a single thing until none are left I guess.
one_or_more(_, [], Acc) -> {Acc, []};
one_or_more(F, [H|T], Acc) ->
    case F(H) of
        true -> one_or_more(F, T, [H|Acc]);
        false -> {Acc, [H|T]}
    end.

is_valid_object([open_brace|Rest]) ->
    {Acc, [close_brace]} = one_or_more(fun json:is_any/1, Rest, []),
    %% We can ensure some surrounding condition and scoop up the inner
    %% content to be blob of yet unparsed things.
    io:format("The accumulation is: ~w~n", [Acc]),
    true;
is_valid_object(_) -> false.

is_balanced_braces (L) ->
    Open = [X || X = open_brace <- L],
    Closed = [X || X = close_brace <- L],
    length(Open) == length(Closed).

is_balanced_quotes (L) ->
    Quotes = [X || X = quote <- L],
    0 =:= length(Quotes) rem 2.

test_is_valid_object () ->
    Ast = parser("{dog}"),
    true = is_valid_object(Ast).

test_is_not_valid_object () ->
    try
        Ast = parser("{dog"),
        true = is_valid_object(Ast)
    of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.

%% partition-by - like explode on a string, but works on lists.
%% Essentially turns a list into a list of lists (opposite of flatten?)
partition_by(_, [], Acc, Tmp) -> [Tmp|Acc];
partition_by(X, [H|T], Acc, Tmp) when X == H ->
    partition_by(X, T, [lists:reverse(Tmp)|Acc], []);
partition_by(X, [H|T], Acc, Tmp) when X /= H ->
    partition_by(X, T, Acc, [H|Tmp]).

partition_by(X, L) -> partition_by(X, L, [], []).

make_key(quote, quote, Inner) -> {inner, Inner}.
make_val(Inner) -> {inner, Inner}.

parse_val(L) ->
    make_val(parse_thing(L)).

%% Should be some quoted string
parse_key(L) ->
    [First|Rest] = L,
    Last = lists:last(Rest),
    Inner = lists:droplast(Rest),
    make_key(First, Last, Inner).

%% Here, we should split by colon and for each thing follow up by
%% making it into some valid key/value.
parse_keyval(L) ->
    [Key, Val] = partition_by(colon, L),
    {key, parse_key(Key), val, parse_val(Val)}.

parse_keyvals(L) ->
    KeyVals = partition_by(comma, L),
    io:format("The keyvals are: ~w~n", [KeyVals]),
    lists:map(fun parse_keyval/1, KeyVals).

%% If we know we have an object, it can create keyvals
make_object(open_brace, close_brace, Inner) ->
    {object, {keyvals, parse_keyvals(Inner)}}.

%% Lets try a list where we just work off first and last parts.
parse_object(L) ->
    %% [First|Rest] = L,
    [First|Rest] = [X || X <- L, X /= ws],
    Last = lists:last(Rest),
    Inner = lists:droplast(Rest),
    make_object(First, Last, Inner).

parse_string(L) -> {string, L}.

%% This is likely a list of any (chars)
parse_any(L) ->
    %% list comprehension, yay
    lists:reverse([X || {any, X} <- L]).

parse_thing([open_brace|T]) -> parse_object([open_brace|T]);
parse_thing([quote|T]) -> parse_string([quote|T]);
parse_thing([{any,X}|T]) -> parse_any([{any,X}|T]).

test_make_object() ->
    parse_object(parser("{ 'one' : 123 }")).
