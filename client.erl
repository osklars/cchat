-module(client).
-export([handle/2, initial_state/2]).
-include_lib("./defs.hrl").

%% inititial_state/2 and handle/2 are used together with the genserver module,
%% explained in the lecture about Generic server.

%% Produce initial state
initial_state(Nick, GUIName) ->
	#client_st{nick=Nick, gui=GUIName, server=empty, rooms=empty}.

%% ---------------------------------------------------------------------------

%% handle/2 handles each kind of request from GUI

%% All requests are processed by handle/2 receiving the request data (and the
%% current state), performing the needed actions, and returning a tuple
%% {reply, Reply, NewState}, where Reply is the reply to be sent to the
%% requesting process and NewState is the new state of the client.

%% Connect to server
handle(St, {connect, Server}) ->
	ServerName=list_to_atom(Server),
	case #St.server of
		empty -> 
			Ref=make_ref(),
			Response=catch(genserver:request(ServerName, {connect, nick, Ref, self()})),
			case Response of
				{response, ok, Ref} -> 
					NewState=St#client_st{server=ServerName},
					{reply, ok, NewState};
				{response, Error, Ref} -> 
					{reply, {error, Error, "Nick taken ~n"}, St};
				_ -> 
					{reply, {error, server_not_reached, "Weird error ~n"}, St}
			end;
		CurrentServer -> 
			{reply, {error, user_already_connected, "You have a server! ~n"}, St}
	end;






%% Disconnect from server
handle(St, disconnect) ->
	case #St.server of
		empty ->
			{reply, {error, user_not_connected, "You don't have a server!~n"}, St}
		ServerName -> 
			Ref=make_ref(),
			Response=catch(genserver:request(list_to_atom(Server), {disconnect, nick, Ref, self()})),
			case Response of
				{response, ok, Ref} ->
					NewState=St#client_st{server=empty},
					{reply, ok, NewState};
				{response, Error, Ref} ->
					{reply, {error, Error, "Leave rooms first! ~n"}, St};
				_ -> 
					{reply, {error, server_not_reached, "Weird error ~n"}, St}
			



handle(St, {connect, Server}) ->
	Data = "hello?",
	io:fwrite("Client is sending: ~p~n", [Data]),
	ServerAtom = list_to_atom(Server),
	Response = genserver:request(ServerAtom, Data),
	io:fwrite("Client received: ~p~n", [Response]),
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

handle(St, disconnect) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

% Join channel
handle(St, {join, Channel}) ->
	% {reply, ok, St} ;
	{reply, {error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
handle(St, {leave, Channel}) ->
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

%% Incoming message
handle(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
	gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
	{reply, ok, St}.
