% Top level module
-module(cchat).
-export([server/0,client/0,start/0,start2/0,send_job/3]).
-include_lib("./defs.hrl").

%% Start a server
server() ->
    Server = "shire",
    genserver:start(list_to_atom(Server), server:initial_state(Server), fun server:handle/2).

%% Start a client GUI
client() ->
    gui:start().

%% Start local server and one client
start() ->
    server(),
    client().

%% Start local server and two clients
start2() ->
    server(),
    client(),
    client().

%% --- Workers ----

send_job(Server, Function, Data) ->
  Ref = make_ref(),
  genserver:request(list_to_atom(Server), {job, Ref, self(), Function, Data}),
  receive
    {job_ans, Ref, Result} -> Result
  end.
