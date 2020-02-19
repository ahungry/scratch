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
            io:format("Evaluation result of promise was: ~p~n", [X]),
            RecvPid ! {promise_result, X},
            evaluator(RecvPid);

        _ -> evaluator(RecvPid)
    end.

all(Fs) ->
    FinalPid = self(),
    RecvPid = spawn(promise, receiver, [[], length(Fs), FinalPid]),
    EvalPid = spawn(promise, evaluator, [RecvPid]),
    lists:map(fun (F) -> EvalPid ! {eval, F} end, Fs),
    receive
        {final, Xs} -> lists:reverse(Xs)
    after 1000 ->
            timeout
    end.

test() ->
    Xs = all([
              fun () -> 1 end,
              fun () -> 2 end,
              fun () -> 3 end
             ]),
    Xs.
