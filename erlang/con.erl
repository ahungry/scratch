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

receiver() ->
    receive
        Anything -> io:format("We got something from dolphin pid! ~p~n", [Anything]),
                    receiver()
    end.

test_d3() ->
    RecvPid = spawn(con, receiver, []),
    DolphinPid = spawn(con, dolphin3, [RecvPid]),
    DolphinPid ! {self(), do_a_flip},
    DolphinPid ! {self(), fish},
    DolphinPid ! {self(), terminate}.
