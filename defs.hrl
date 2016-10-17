% This record defines the structure of the client process.
% It contains the following fields:
%   gui: the name of the GUI process.
% 	nick: nick of client.
% 	server: empty or server atom.
% 	rooms: List of room names.
-record(client_st, {gui, nick, server, rooms}).

% This record defines the structure of the server process.
% It contains the following fields:
% 	name: genserver registered name of server.
% 	clients: list of clients {nick, pid}.
% 	rooms: list of {roomName,[clientnick1,...,clientnickn]}.
-record(server_st, {name, clients, rooms}).

% This record defines the structure of the room process.
% It contains the following fields:
% 	name: genserver registered name of room.
% 	clients: list of clients {nick, pid}.
-record(room_st, {name, clients}).
