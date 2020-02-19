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
    lists:map(fun (F) ->
                      EvalPid = spawn(promise, evaluator, [RecvPid]),
                      EvalPid ! {eval, F} ,
                      exit(EvalPid, normal)
              end, Fs),
    Res = receive
        {final, Xs} -> Xs
    after 5000 ->
            io:format("Timeout..."), []
    end,
    exit(RecvPid, normal),
    lists:reverse(Res).

test() ->
    Xs = all([
              fun () -> timer:sleep(1000), 1 end,
              fun () -> timer:sleep(1000), 2 end,
              fun () -> timer:sleep(1000), 3 end,
              fun () -> timer:sleep(1000), 4 end
             ]),
    Xs.
