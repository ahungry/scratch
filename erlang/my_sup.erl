% https://riptutorial.com/erlang/example/23589/basic-supervisor-with-one-worker-process
% https://marketsplash.com/tutorials/all/erlang-supervisors/
% https://learnyousomeerlang.com/building-applications-with-otp
% https://www.erlang.org/doc/design_principles/des_princ
% https://github.com/yowcow/erlang-supervisor

-module(my_sup).

-behaviour(supervisor).

% API Exports
-export([start_link/0]).

% Behaviour exports
-export([init/1]).

start_link() ->
    Args = [],
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(_Args) ->
    SupFlags = #{strategy => rest_for_one,
               intensity => 1,
               period => 5},

    Child = #{id => my_worker,
             start => {my_worker, start_link, []}},

    Children = [Child],

    {ok, {SupFlags, Children}}.
