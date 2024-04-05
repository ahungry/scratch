% https://stackoverflow.com/questions/20339937/getting-result-of-a-spawned-function-in-erlang
% Use M-x run-erlang to start an erl shell
% Then use:
%   c(concurrency).
% To compile it - then cal it with:
%   concurrency:calc(9).

-module(concurrency).

-export([
         calc/1,
         start/0
        ]).
%% -import(io, [format/1]).
%% -import(init, [get_plain_arguments/0]).

fact(X) -> fact(X, 1).

fact(0, R) -> R;
fact(X, R) when X > 0 -> fact(X-1, R*X).

calc(N) ->
    Self = self(),
    Pids = [ spawn_link(fun() -> Self ! {self(), {X, fact(X)}} end)
            || X <- lists:seq(1, N) ],
    [ receive {Pid, R} -> R end || Pid <- Pids ].

start() ->
    io:format("Factorials~n"),
    [N] = init:get_plain_arguments(),
    I = list_to_integer(N),
    Res = calc(I),
    % io:format("~p", [Args]),
    io:format("~p~n", [Res]).
