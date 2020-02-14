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
