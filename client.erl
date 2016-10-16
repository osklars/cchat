-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used together with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
	#client_st{nick=list_to_atom(Nick), gui=GUIName, server=empty, rooms=[]}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	ServerName=list_to_atom(Server),
	case St#client_st.server of
		empty -> 
			Response=genserver:request(ServerName, {connect, St#client_st.nick}), % add catch?
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
handle(St, disconnect) ->
	case St#client_st.server of
		empty ->
			{reply, {error, user_not_connected, "You don't have a server!"}, St};
		ServerName -> 
			Response=genserver:request(ServerName, {disconnect, St#client_st.nick}), % add catch?
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

handle(St, {join, Channel}) ->
	Ch=list_to_atom(Channel),
	case lists:member(Ch, St#client_st.rooms) of
		true -> {reply, {error, user_already_joined, "You're in the room already!"}, St};
		false -> 
			Response=genserver:request(St#client_st.server, {join, Ch, St#client_st.nick}),
			case Response of
				ok ->
					NewState=St#client_st{rooms=St#client_st.rooms++[Ch]},
					{reply, ok, NewState};
				_ -> {reply, {error, server_not_reached, "Weird error"}, St}
			end
	end;
			
handle(St, {leave, Channel}) ->
	Ch=list_to_atom(Channel),
	case lists:member(Ch, St#client_st.rooms) of
		true ->
			Response=genserver:request(St#client_st.server, {leave, Ch, St#client_st.nick}),
			case Response of
				ok ->
					NewState=St#client_st{rooms=lists:delete(Ch,St#client_st.rooms)},
					{reply, ok, NewState};
				_ -> {reply, {error, server_not_reached, "Weird error"}, St}
			end;
		false -> {reply, {error, user_not_joined, "You're not in this room"}, St}
	end;



%%%%%%%%%%%% example code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(St, {connecta, Server}) ->
	Data = "hello?",
	io:fwrite("Client is sending: ~p~n", [Data]),
	ServerAtom = list_to_atom(Server),
	Response = genserver:request(ServerAtom, Data),
	io:fwrite("Client received: ~p~n", [Response]),
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

handle(St, disconnecta) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
handle(St, {joina, Channel}) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leavea, Channel}) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

% Sending messages
handle(St, {msg_from_GUI, Channel, Msg}) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
handle(St, whoami) ->
	% {reply, "nick", St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Change nick
handle(St, {nick, Nick}) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Incoming message %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
	gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
	{reply, ok, St}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



