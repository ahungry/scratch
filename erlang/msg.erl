-module(msg).
-export([launcher/0]).

hello() ->
    io:format("Hello and goodbye~n").

launcher() ->
    hello(),
    spawn(fun() -> hello(), io:format("The child sent this, it's pid was: [~p]~n.", [self()]) end),
    io:format("About to do the list comprehensions...~n"),
    Self = self(),
    Pids = [spawn_link(fun() -> Self ! {self(), N + 1} end) || N <- lists:seq(0, 20)],
    Rs = [receive {Pid, R} -> R end || Pid <- Pids],
    io:format("Received: ~p", [Rs]).
