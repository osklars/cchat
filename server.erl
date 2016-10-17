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


handle(St, {connect, Client={Nick, _}}) ->
	case lists:keymember(Nick, 1, St#server_st.clients) of
		true -> 
			{reply, nick_taken, St};
		false -> 
			NewState=St#server_st{clients=St#server_st.clients++[Client]},
			{reply, ok, NewState}
	end;

handle(St, {disconnect, Client}) ->
	case clientRoomMatch(Client, St#server_st.rooms) of
		true -> 
			{reply, leave_channels_first, St};
		false -> 
			NewState=St#server_st{clients=lists:delete(Client, St#server_st.clients)},
			{reply, ok, NewState}
	end;

handle(St, {join, Channel, Client}) -> 
	case isRoom(Channel, St#server_st.rooms) of
		false -> 
			genserver:start(Channel, room:initial_state(Channel, Client), fun room:handle/2),
			NewState=St#server_st{rooms=St#server_st.rooms++[{Channel, [Client]}]},
			{reply, ok, NewState};
		Room -> 
			Response=genserver:request(Channel, {join, Client}),
			case Response of
				ok ->
					NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, Room++[Client]})},
					{reply, ok, NewState};
				_ -> 
					{reply, error, St}
			end
	end;

handle(St, {leave, Channel, Client}) ->
	Response=genserver:request(Channel, {leave, Client}),
	case Response of
		ok ->
			NewRoom=lists:delete(Client, isRoom(Channel, St#server_st.rooms)),
			NewState=St#server_st{rooms=lists:keyreplace(Channel, 1, St#server_st.rooms, {Channel, NewRoom})},
			{reply, ok, NewState};
		_ -> 
			{reply, error, St}
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
