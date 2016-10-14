-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used togetger with the genserver module,
%% explained in the lecture about Generic server.

% Produce initial state
initial_state(ServerName) ->
	#server_st{name=ServerName, clients=[], rooms=[]}.

%% ---------------------------------------------------------------------------

%% handle/2 handles requests from clients

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the client
%% and NewState is the new state of the server.


handle(St, {connect, nick, Ref, From}) ->
	case match(nick, clients) of
		true -> From ! {reply, nick_taken, Ref};
		false -> NewState=St#server_st{clients=#St.clients++[nick]},
			From ! {reply, ok, Ref}
	end;

handle(St, {disconnect, nick, Ref, From}) ->
	case match(nick, clients)

handle(St, Request) ->
	io:fwrite("Server received: ~p~n", [Request]),
	Response = "hi!",
	io:fwrite("Server is sending: ~p~n", [Response]),
	{reply, Response, St}.




match(P,[L|LS]) -> case match(P,L) of
                              true -> match(P,LS);
                              false -> false
                         end;
match(P,P) -> true;
match(_,_) -> false.

