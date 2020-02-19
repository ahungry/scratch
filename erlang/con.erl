-module(con).
-compile(export_all).

dolphin3() ->
    receive
        {From, do_a_flip} ->
            From ! "How about no?",
            dolphin3();
        {From, fish} ->
            From ! "So long and thanks for all the fish!";
        _ -> io:format("Heh, we're smarter than you humans.~n"),
             dolphin3()
    end.
