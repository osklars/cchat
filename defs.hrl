% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
% 	nick: nick of client.
% 	server: empty or server atom.
% 	rooms: empty or List of room names.
-record(client_st, {gui, nick, server, rooms}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
% It contains the following fields:
% 	clients: empty or list of clients.
% 	rooms: empty or list of roomNames=[clientnick1,...,clientnickn].
-record(server_st, {name, clients, rooms}).
