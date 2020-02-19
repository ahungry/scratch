%% http://erlang.ahungry.com/the-hitchhikers-guide-to-concurrency.html#dont-panic

-module(con).
-compile(export_all).

dolphin3(RecvPid) ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            RecvPid ! "How about no? (recv)",
            dolphin3(RecvPid);

        {From, fish} ->
            From ! "So long and thanks for all the fish!",
            RecvPid ! "So long and thanks for all the fish! (recv)";

        _ -> io:format("Heh, we're smarter than you humans.~n"),
             dolphin3(RecvPid)
    end.

%% See whats pending with flush()
%% Make a new pid with Pid=spawn(module_name, function_name, [init_args])
%% Rest vars with f()

receiver(Acc, ExpectedCount, FinalPid) when length(Acc) < ExpectedCount ->
    receive
        Anything -> io:format("We got something from dolphin pid! ~p~n", [Anything]),
                    receiver([Anything|Acc], ExpectedCount, FinalPid)
    end;
receiver(Acc, _, FinalPid) -> FinalPid ! {final, Acc}.

test_d3() ->
    FinalPid = self(),
    RecvPid = spawn(con, receiver, [[], 2, FinalPid]),
    DolphinPid = spawn(con, dolphin3, [RecvPid]),
    DolphinPid ! {self(), do_a_flip},
    DolphinPid ! {self(), fish},
    DolphinPid ! {self(), terminate},
    %% This will stall out and wait for the match - I guess it could fill up
    %% with message passes we don't actually care about since we aren't handling
    %% then in our receive block - I guess we would want to put this in its own spawn
    %% later.
    receive
        {final, Anything} -> io:format("At the end, FinalPid called! ~p~n", [Anything])
    end.
