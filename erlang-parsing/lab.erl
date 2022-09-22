% See: https://stackoverflow.com/questions/11013618/scanning-big-binary-with-erlang

-module(lab).
-compile(export_all).

find(BinPattern, InputFile) ->
    BinPatternLength = length(binary_to_list(BinPattern)),
    {ok, S} = file:open(InputFile, [read, binary, raw]),
    loop(S, BinPattern, 0, BinPatternLength, 0),
    file:close(S),
    io:format("Done!~n", []).

loop(S, BinPattern, StartPos, Length, Acc) ->
    case file:pread(S, StartPos, Length) of
    {ok, Bin} ->
        case Bin of
        BinPattern ->
            io:format("Found one at position: ~p.~n", [StartPos]),
            loop(S, BinPattern, StartPos + 1, Length, Acc + 1);
        _ ->
            loop(S, BinPattern, StartPos + 1, Length, Acc)
        end;
    eof ->
        io:format("I've proudly found ~p matches:)~n", [Acc])
    end.

start() ->
    lab:find(<<"abc">>, "./test.txt").

%% 1> c(lab).
%% {ok,lab}
%% 2> lab:find(<<"abc">>, "./test.txt").
%% Found one at position: 43.
%% Found one at position: 103.
%% I've proudly found 2 matches:)
%% Done!
%% ok
