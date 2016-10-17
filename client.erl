-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used together with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
	#client_st{nick=list_to_atom(Nick), gui=GUIName, server=empty, rooms=[]}.

%% ------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.



%% Connect to server

% Input: 
% 	Server - String of server name.
% Output: 
% 	ok - Client successfully connected to server.
% Errors: 
% 	nick_taken - Nick already occupied in server.
% 	user_already_connected - Client already in server.
% 	server_not_reached - Connection error.

% Checks if client not already in server. If so passes request to server. 
% Updates record if connection completed.
handle(St, {connect, Server}) ->
	ServerName=list_to_atom(Server),
	case St#client_st.server of
		empty -> 
			Response=(catch genserver:request(ServerName, {connect, {St#client_st.nick, self()}})), 
			case Response of
				ok -> 
					NewState=St#client_st{server=ServerName},
					{reply, ok, NewState};
				nick_taken -> 
					{reply, {error, nick_taken, "Nick taken"}, St};
				_ -> 
					{reply, {error, server_not_reached, "Weird error"}, St}
			end;
		_ -> 
			{reply, {error, user_already_connected, "You have a server!"}, St}
	end;


%% Disconnect from server

% Output:
% 	ok - Client successfully disconnected from server.
% Errors:
% 	user_not_connected - Client not in any server.
% 	leave_channels_first - Client in rooms.
% 	server_not_reached - Connection error.

% Checks if client connected to any server. If so passes request to server.
% Updates record if disconnection completed.
handle(St, disconnect) ->
	case St#client_st.server of
		empty ->
			{reply, {error, user_not_connected, "You don't have a server!"}, St};
		ServerName -> 
			Response=genserver:request(ServerName, {disconnect, {St#client_st.nick, self()}}), % add catch?
			case Response of
				ok ->
					NewState=St#client_st{server=empty},
					{reply, ok, NewState};
				leave_channels_first ->
					{reply, {error, leave_channels_first, "Leave rooms first!"}, St};
				_ -> 
					{reply, {error, server_not_reached, "Weird error"}, St}
			end
	end;


%% Join room

% Output:
% 	ok - Client succesfully joined room.
% Errors:
% 	user_not_connected - Client not in any server.
% 	leave_channels_first - Client in rooms.
% 	server_not_reached - Connection error.

% Checks if client connected to any server. If so passes request to server.
% Updates record if disconnection completed.
handle(St, {join, Channel}) ->
	Ch=list_to_atom(Channel),
	case lists:member(Ch, St#client_st.rooms) of
		true -> 
			{reply, {error, user_already_joined, "You're in the room already!"}, St};
		false -> 
			Response=genserver:request(St#client_st.server, {join, Ch, {St#client_st.nick, self()}}),
			case Response of
				ok ->
					NewState=St#client_st{rooms=St#client_st.rooms++[Ch]},
					{reply, ok, NewState};
				_ -> 
					{reply, {error, server_not_reached, "Weird error"}, St}
			end
	end;

%% Leave room			
handle(St, {leave, Channel}) ->
	Ch=list_to_atom(Channel),
	case lists:member(Ch, St#client_st.rooms) of
		true ->
			Response=genserver:request(St#client_st.server, {leave, Ch, {St#client_st.nick, self()}}),
			case Response of
				ok ->
					NewState=St#client_st{rooms=lists:delete(Ch,St#client_st.rooms)},
					{reply, ok, NewState};
				_ -> 
					{reply, {error, server_not_reached, "Weird error"}, St}
			end;
		false -> 
			{reply, {error, user_not_joined, "You're not in this room"}, St}
	end;

%% Send message
handle(St, {msg_from_GUI, Channel, Msg}) -> 
	Ch=list_to_atom(Channel),
	case lists:member(Ch, St#client_st.rooms) of
		true ->
			Response=genserver:request(Ch, {msg_from_GUI, Ch, St#client_st.nick, Msg}),
			case Response of
				ok -> {reply, ok, St};
				_ -> {reply, {error, server_not_reached, "Weird error"}, St}
			end;
		false ->
			{reply, {error, user_not_joined, "How did you do this with GUI?!?!?"}, St}
	end;

%% Get nick
handle(St, whoami) ->
	{reply, St#client_st.nick, St};

%% Set nick
handle(St, {nick, Nick}) ->
	Name=list_to_atom(Nick),
	case St#client_st.server of
		empty -> 
			NewState=St#client_st{nick=Name},
			{reply, ok, NewState};
		_ -> 
			{reply, {error, user_already_connected, "Disconnect before renaming!"}, St}
	end;

%% Incoming message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(St=#client_st{gui=GUIName}, {incoming_msg, Channel, Name, Msg}) ->
	gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
	{reply, ok, St}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
