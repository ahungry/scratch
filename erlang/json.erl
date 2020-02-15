-module(json).
-compile(export_all).

%% Try to write a JSON parser in erlang (few days xp with it).
%% Read the strings, make something to represent them.
parse("{" ++ Rest) -> {open_brace, Rest};
parse("}" ++ Rest) -> {close_brace, Rest};
parse("[" ++ Rest) -> {open_rect, Rest};
parse("]" ++ Rest) -> {close_rect, Rest};
parse(" " ++ Rest) -> {ws, Rest};
parse("
" ++ Rest) -> {ws, Rest};
parse("'" ++ Rest) -> {quote, Rest};
parse(":" ++ Rest) -> {colon, Rest};
parse("," ++ Rest) -> {comma, Rest};
parse("")          -> eot;
parse([H|T])       -> {{any, H}, T}.

parser(S) -> parser(parse(S), []).

parser(eot, Acc) -> lists:reverse(Acc);
%% TODO: Need to keep track of when we're inside strings
%% As we parse, so until we close string, each thing encountered inside
%% is flagged as a string_colon or string_quote etc.
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
%% We should allow tracking if we 'hit' inside something that disables the
%% partitioning - if we "enter" braces or quotes.
partition_by(_, [], Acc, Tmp) -> lists:reverse([lists:reverse(Tmp)|Acc]);
partition_by(X, [H|T], Acc, Tmp) when X == H ->
    partition_by(X, T, [lists:reverse(Tmp)|Acc], []);
partition_by(X, [H|T], Acc, Tmp) when X /= H ->
    partition_by(X, T, Acc, [H|Tmp]).

partition_by(X, L) -> partition_by(X, L, [], []).

test_partition_by() ->
    [[1, 2], [3, 4]] = partition_by(bbb, [1, 2, bbb, 3, 4]).

%% Find each char between two things and "mask" it.
mask_between(Orig, Mask, Start, End, Masking, [], Acc) -> lists:reverse(Acc);
mask_between(Orig, Mask, Start, End, Masking, [H|T], Acc)
  when Start == H ->
    mask_between(Orig, Mask, Start, End, true, T, [H|Acc]);
mask_between(Orig, Mask, Start, End, Masking, [H|T], Acc)
  when End == H ->
    mask_between(Orig, Mask, Start, End, false, T, [H|Acc]);
mask_between(Orig, Mask, Start, End, Masking, [H|T], Acc)
  when true == Masking, H == Orig ->
    mask_between(Orig, Mask, Start, End, Masking, T, [Mask|Acc]);
mask_between(Orig, Mask, Start, End, Masking, [H|T], Acc) ->
    mask_between(Orig, Mask, Start, End, Masking, T, [H|Acc]).

test_mask_between() ->
    Res = mask_between(
          dog,
          mdog,
          a,
          z,
          false,
          [1, 2, a, dog, dog, z, dog],
          []
         ),
    io:format("We came up with: ~w~n~n", [Res]),
    [1, 2, a, mdog, mdog, z, dog] = Res.

%% Simulate closer to our real use cases
%% Split by comma, then by colon.
test_mask_between2() ->
    Res = mask_between(comma, mcomma, open_brace, close_brace, false,
                       [open_brace, key, colon, open_brace,
                        key, colon, val, comma, key, colon, val,
                        close_brace, close_brace], []),
    io:format("We came up with: ~w~n~n", [Res]).

make_key(quote, quote, Inner) -> parse_any(Inner).
make_val(Inner) -> Inner.

parse_val(L) ->
    %%io:format("A call to parse a val: ~w~n", [L]),
    Unmasked = unmask_colons(unmask_commas(L)),
    make_val(parse_thing(Unmasked)).

%% Should be some quoted string
parse_key(L) ->
    [First|Rest] = L,
    Last = lists:last(Rest),
    Inner = lists:droplast(Rest),
    make_key(First, Last, Inner).

mask_colons(L) ->
    mask_between(colon, mcolon, open_brace, close_brace, false, L, []).

unmask_colons(L) ->
    mask_between(mcolon, colon, open_brace, close_brace, false, L, []).

%% Here, we should split by colon and for each thing follow up by
%% making it into some valid key/value.
parse_keyval(L) ->
    Masked = mask_colons(L),
    %%io:format("The masked colons list is: ~w~n", [Masked]),
    [Key, Val] = partition_by(colon, Masked),
    %%io:format("~nThe masked colons k, v is: ~w and ~w ~n", [Key, Val]),
    {key, parse_key(Key), val, parse_val(Val)}.

mask_commas(L) ->
    L1 = mask_between(comma, mcomma, open_brace, close_brace, false, L, []),
    L2 = mask_between(comma, mcomma, open_rect, close_rect, false, L1, []),
    L2.

unmask_commas(L) ->
    L1 = mask_between(mcomma, comma, open_brace, close_brace, false, L, []),
    L2 = mask_between(mcomma, comma, open_rect, close_rect, false, L1, []),
    L2.

%% Before we split/partition by things, we need to escape some.
parse_keyvals(L) ->
    Masked = mask_commas(L),
    %%io:format("~n~nThe masked input list itself is: ~w~n~n", [Masked]),
    %% KeyVals = unmask_commas(partition_by(comma, Masked)),
    KeyVals = partition_by(comma, Masked),
    lists:map(fun parse_keyval/1, KeyVals).

%% If we know we have an object, it can create keyvals
make_object(open_brace, close_brace, Inner) ->
    {object, parse_keyvals(Inner)}.

%% Lets try a list where we just work off first and last parts.
parse_object(L) ->
    %% [First|Rest] = L,
    [First|Rest] = [X || X <- L, X /= ws],
    Last = lists:last(Rest),
    Inner = lists:droplast(Rest),
    make_object(First, Last, Inner).

make_array(open_rect, close_rect, Inner) ->
    %% Just turn the values into numeric keyed list.
    Vals = partition_by(comma, mask_commas(Inner)),
    %%io:format("The vals are now: ~p~n", [Vals]),
    {array, lists:map(fun parse_val/1, Vals)}.
    %%{array, {vals, lists:map(fun parse_val/1, Inner)}}.

parse_array(L) ->
    %%io:format("parse_array found: ~p~n", [L]),
    [First|Rest] = [X || X <- L, X /= ws],
    Last = lists:last(Rest),
    Inner = lists:droplast(Rest),
    make_array(First, Last, Inner).

parse_string(L) ->
    {string, [X || {any,X} <- L]}.

%% This is likely a list of any (chars)
parse_any(L) ->
    %% list comprehension, yay
    [X || {any, X} <- L].

parse_thing([open_rect|T]) -> parse_array([open_rect|T]);
parse_thing([open_brace|T]) -> parse_object([open_brace|T]);
parse_thing([quote|T]) -> parse_string([quote|T]);
parse_thing([{any,X}|T]) -> parse_any([{any,X}|T]).

test_make_object() ->
    parse_object(parser("
{
  'someString': 'Hello',
  'anArray': [1, 2, 3, {'b': 99, 'c': 100, 'innerArray': [8, 7]}],
  'one' : 123,
  'two': 2,
  'three': {
             'x': 1,
             'y': 2
           }
}
")).
