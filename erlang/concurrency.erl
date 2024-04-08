% https://stackoverflow.com/questions/20339937/getting-result-of-a-spawned-function-in-erlang
% Use M-x run-erlang to start an erl shell
% Then use:
%   c(concurrency).
% To compile it - then cal it with:
%   concurrency:calc(9).

-module(concurrency).

-export([
         calc/1,
         start/0,
         get_url/0
        ]).
%% -import(io, [format/1]).
%% -import(init, [get_plain_arguments/0]).

fact(X) -> fact(X, 1).

fact(0, R) -> R;
fact(X, R) when X > 0 -> fact(X-1, R*X).

% https://www.erlang.org/doc/man/httpc.html
% https://elixirforum.com/t/httpc-cheatsheet/50337
get_url() ->
% https://stackoverflow.com/questions/1839862/erlang-when-to-perform-inetsstart
    inets:start(),
    ssl:start(),
    Url = "http://httpbin.org/ip",
    Headers = [{"accept", "application/json"}],
    % Http_opts = [{ssl, []}],
    Http_opts = [],
    {ok, {_, _, Body}} = httpc:request(get, {Url, Headers}, Http_opts, []),
    Body.

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
