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


handle(St, Request={connect, Client={Nick, _}}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	case lists:keymember(Nick, 1, St#server_st.clients) of
		true -> {reply, nick_taken, St};
		false -> NewState=St#server_st{clients=St#server_st.clients++[Client]},
			{reply, ok, NewState}
	end;

handle(St, Request={disconnect, Client}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	case clientRoomMatch(Client, St#server_st.rooms) of
		true -> {reply, leave_channels_first, St};
		false -> NewState=St#server_st{clients=lists:delete(Client, St#server_st.clients)},
			{reply, ok, NewState}
	end;

handle(St, Request={join, Channel, Client}) -> 
	io:fwrite("Server received: ~p~n", [Request]),
	case isRoom(Channel, St#server_st.rooms) of
		false -> NewState=St#server_st{rooms=St#server_st.rooms++[{Channel, [Client]}]},
			{reply, ok, NewState};
		Room -> NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, Room++[Client]})},
			{reply, ok, NewState}
	end;

handle(St, Request={leave, Channel, Client}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	NewRoom=lists:delete(Client, isRoom(Channel, St#server_st.rooms)),
	NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, NewRoom})},
	{reply, ok, NewState};

handle(St, Request={msg_from_GUI, Channel, Nick, Msg}) ->
	io:fwrite("Server received: ~p~n", [Request]),
	Response=message(Msg, lists:keydelete(Nick, 1, isRoom(Channel, St#server_st.rooms)), Nick, Channel),
	case Response of
		ok -> {reply, ok, St};
		_ -> {reply, server_not_reached, St}
	end;
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% example code %%%%%%%%%%%%%%%%%%%%
handle(St, Request) ->
	io:fwrite("Server received: ~p~n", [Request]),
	Response = "hi!",
	io:fwrite("Server is sending: ~p~n", [Response]),
	{reply, Response, St}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clientRoomMatch(_, []) -> false;
clientRoomMatch(Client, [{_,Clients}|RT]) ->
	case lists:member(Client, Clients) of
		true -> true;
		false -> clientRoomMatch(Client, RT)
	end.

isRoom(_, []) -> false;
isRoom(Channel, [{Channel, Room}|_]) -> Room;
isRoom(Channel, [_|RT]) -> isRoom(Channel, RT).

message(_, [], _, _) -> ok;
message(Msg, [{_, Pid}|Room], Name, Channel) ->
	Response=genserver:request(Pid, {incoming_msg, atom_to_list(Channel), atom_to_list(Name), Msg}),
	case Response of
		ok -> message(Msg, Room, Name, Channel);
		_ -> server_not_reached
	end.








