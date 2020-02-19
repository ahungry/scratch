-module(promise).
-compile(export_all).

receiver(Acc, Count, FinalPid) when length(Acc) < Count ->
    receive
        {promise_result, Val} -> receiver([Val|Acc], Count, FinalPid)
    end;
receiver(Acc, _, FinalPid) -> FinalPid ! {final, Acc}.

evaluator(RecvPid) ->
    receive
        {eval, F} ->
            X = F(),
            %%io:format("Evaluation result of promise was: ~p~n", [X]),
            RecvPid ! {promise_result, X},
            evaluator(RecvPid);

        _ -> evaluator(RecvPid)
    end.

all(Fs) ->
    FinalPid = self(),
    RecvPid = spawn(promise, receiver, [[], length(Fs), FinalPid]),
    lists:map(fun (F) ->
                      EvalPid = spawn(promise, evaluator, [RecvPid]),
                      EvalPid ! {eval, F} ,
                      exit(EvalPid, normal)
              end, Fs),
    Res = receive
        {final, Xs} -> Xs
    after 15000 ->
            io:format("Timeout..."), []
    end,
    exit(RecvPid, normal),
    lists:reverse(Res).

%% Returns a function that will return X after some time
get_fn(X) -> fun () -> timer:sleep(1000), X end.

test() ->
    Start = erlang:timestamp(),
    Xs = all(lists:map(fun get_fn/1, lists:seq(1, 10000))),
    io:format("Done with the evaluation result, found ~w items ~n", [length(Xs)]),
    End = erlang:timestamp(),
    Diff = timer:now_diff(End, Start) / 1000 / 1000,
    io:format("It took some time: ~p~n", [Diff]),
    done.
