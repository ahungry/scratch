-module(json).
-compile(export_all).

%% Try to write a JSON parser in erlang (few days xp with it).
%% Read the strings, make something to represent them.
parse("{" ++ Rest) -> {open_brace, Rest};
parse("}" ++ Rest) -> {close_brace, Rest};
parse("")       -> eot;
parse([H|T])    -> {{any, H}, T}.

parser(S) -> parser(parse(S), []).

parser(eot, Acc) -> lists:reverse(Acc);
parser({Sym, Rest}, Acc) ->
    parser(parse(Rest), [Sym|Acc]).

is_any({any, _}) -> true;
is_any(_) -> false.

%% Iterate on a single thing until none are left I guess.
one_or_more(_, []) -> [];
one_or_more(F, [H|T]) ->
    case F(H) of
        true -> one_or_more(F, T);
        false -> [H|T]
    end.

is_valid_object([open_brace|Rest]) ->
    [close_brace] = one_or_more(fun json:is_any/1, Rest),
    true;
is_valid_object(_) -> false.

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
