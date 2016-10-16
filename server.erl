-module(server).
-export([handle/2, initial_state/1]).
-include_lib("./defs.hrl").

%% inititial_state/1 and handle/2 are used togetger with the genserver module,
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


handle(St, Request={connect, Nick}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	case lists:member(Nick, St#server_st.clients) of
		true -> {reply, nick_taken, St};
		false -> NewState=St#server_st{clients=St#server_st.clients++[Nick]},
			{reply, ok, NewState}
	end;

handle(St, Request={disconnect, Nick}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	case nickRoomMatch(Nick, St#server_st.rooms) of
		true -> {reply, leave_channels_first, St};
		false -> NewState=St#server_st{clients=lists:delete(Nick, St#server_st.clients)},
			{reply, ok, NewState}
	end;

handle(St, Request={join, Channel, Nick}) -> % might happen that Nick is list, hope not.
	io:fwrite("Server received: ~p~n", [Request]),
	case isRoom(Channel, St#server_st.rooms) of
		false -> NewState=St#server_st{rooms=St#server_st.rooms++[{Channel, [Nick]}]},
			{reply, ok, NewState};
		Room -> NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, Room++[Nick]})},
			{reply, ok, NewState}
	end;

handle(St, Request={leave, Channel, Nick}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	NewRoom=lists:delete(Nick, isRoom(Channel, St#server_st.rooms)),
	NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, NewRoom})},
	{reply, ok, NewState};
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% example code %%%%%%%%%%%%%%%%%%%%
handle(St, Request) ->
	io:fwrite("Server received: ~p~n", [Request]),
	Response = "hi!",
	io:fwrite("Server is sending: ~p~n", [Response]),
	{reply, Response, St}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nickRoomMatch(_, []) -> false;
nickRoomMatch(Nick, [{_,Clients}|RT]) ->
	case lists:member(Nick, Clients) of
		true -> true;
		false -> nickRoomMatch(Nick, RT)
	end.

isRoom(_, []) -> false;
isRoom(Channel, [{Channel, Room}|_]) -> Room;
isRoom(Channel, [_|RT]) -> isRoom(Channel, RT).
