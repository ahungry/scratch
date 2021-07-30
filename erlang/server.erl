%% -*- erlang-indent-level: 2 -*-

%% Compile directly with "c(server)."

-module(server).
-author("Me").

-export([
start/0
]).

%% Import some file stuff, common list: module things.
-import(io, [format/1]).
-import(gen_tcp, []).

%% Same as using flags -export_all and -debug_info
%% TODO: Do not keep export_all on prod stuff.
-compile([debug_info, export_all]).

start () ->
  format("Listening\n"),
  server(),
  ok.

server() ->
  {ok, LSock} = gen_tcp:listen(10030, [binary, {packet, 0}, {active, false}]),
  {ok, Sock} = gen_tcp:accept(LSock),
  {ok, Bin} = do_recv(Sock, []),
  gen_tcp:send(Sock, Bin),
  ok = gen_tcp:close(Sock),
  ok = gen_tcp:close(LSock),
  Bin.

do_recv(Sock, Bs) ->
  case gen_tcp:recv(Sock, 0) of
    {ok, B} ->
      do_recv(Sock, [Bs, B]);
    {error, closed} ->
      {ok, list_to_binary(Bs)}
  end.
