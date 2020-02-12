-module(ex).
-compile(export_all).

%% https://learnyousomeerlang.com/errors-and-exceptions

throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw, caught, Throw}
    end.

errors(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error}
    end.

exits(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.

%% looks for a given value 'Val' in the tree.
has_value(_, {node, 'nil'}) ->
    false;
has_value(Val, {node, {_, Val, _, _}}) ->
    true;
has_value(Val, {node, {_, _, Left, Right}}) ->
    case has_value(Val, Left) of
        true -> true;
        false -> has_value(Val, Right)
    end.

test_has_value() ->
    Nil = {node, 'nil'},
    X = {node, {foo, 32, Nil, Nil}},
    has_value(32, X).
