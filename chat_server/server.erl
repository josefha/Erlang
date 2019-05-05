
-module(server).

-export([start/1]).

-import(dict,[new/0,find/2,append/3,fetch_keys/1,erase/2]).

% Starts the server
% args: LPort is the choosen port number
start(LPort) ->
    case gen_tcp:listen(LPort,[{active, false},{packet,2}]) of
      {ok, ListenSock} ->
          ServerPID = self(),
          open_server_commands(ServerPID),
          start_client_node(ListenSock, ServerPID),
          {ok, _} = inet:port(ListenSock),

          print("--- Server started ---"),
          %% UserName -> CientHandlerPID
          ActiveUsers = dict:new(),
          %% RoomID -> [CientHandlerPID,..]
          Rooms = dict:new(),
          %% setup main loop

          main_loop(ActiveUsers,Rooms),
          ok = gen_tcp:close(ListenSock);

      {error,Reason} ->
          print2("Server starting error: ", Reason),
          error
    end.


% Starts an actor that handles the input from
% the server terminal, if text matches an
% command a message is sent to ServerPID
open_server_commands(ServerPID) ->
  spawn(fun () -> server_commands(ServerPID) end).

% Gets input from the server terminal
server_commands(ServerPID) ->
  Line = string:strip(io:get_line(""), right, $\n),
  case Line of
    "quit" -> ServerPID ! {quit};
    "user_count" -> ServerPID ! {user_count};
    _ -> nothing
  end,
  server_commands(ServerPID).

% Main loop of the server node
% it handles messages from client nodes
% and messages sent from the server terminal
% The state is storad as argumnts by these hash maps:
%   ActiveUsers contain usernames as keys and pids as values
%   Rooms contain rommids as keys and a list of username as values
main_loop(ActiveUsers,Rooms) ->
  receive
      %% --- Client Commands ---- %%
      {register, UserName, PID} ->
            ActiveUsers_ = dict:append(UserName, PID, ActiveUsers),
            print2(UserName,"connected"),
            main_loop(ActiveUsers_,Rooms);

      {list_users, PID} ->
            PID ! {fetch_keys(ActiveUsers)},
            main_loop(ActiveUsers,Rooms);

      {logout, UserName} ->
           print2(UserName, "disconnected"),
           ActiveUsers_ = dict:erase(UserName,ActiveUsers),
           main_loop(ActiveUsers_,Rooms);

      {connect, UserName, Friend, PID} ->
           case dict:find(Friend, ActiveUsers) of
              {ok, [FriendPID]} ->
                                io:format("~p started a chat with ~p ~n", [UserName,Friend]),
                                RoomID = UserName ++ "-" ++ Friend,
                                FriendPID ! {chat_request, RoomID, UserName},
                                PID ! {chat_started, RoomID, Friend},

                                Rooms_ = dict:append_list(RoomID, [UserName, Friend], Rooms),
                                main_loop(ActiveUsers,Rooms_);
              error ->  print2("no user found named ", Friend),
                        PID ! {chat_dismissal, Friend},
                        main_loop(ActiveUsers,Rooms)
           end;

      {msg, RoomID, Msg, _} ->
        case dict:find(RoomID, Rooms) of
           {ok, Users} ->
               send_msg_to(Users, ActiveUsers, RoomID, Msg),
               main_loop(ActiveUsers,Rooms);
            error ->
               print("Error: chat is closed"),
               main_loop(ActiveUsers,Rooms)
        end;

      {start_group, Users, RoomID, UserName, _} ->
          io:format("~p started a group named ~p ~n", [UserName,RoomID]),
          notify_users(Users, ActiveUsers, {chat_request, RoomID, UserName}),
          Rooms_ = dict:append_list(RoomID, Users, Rooms),
          main_loop(ActiveUsers,Rooms_);

      {invite, Friend, RoomID, UserName} ->
          case dict:find(Friend, ActiveUsers) of
             {ok, [FriendPID]} ->
                       io:format("~p invited ~p to join ~p ~n", [UserName,Friend,RoomID]),
                       FriendPID ! {chat_started, RoomID, UserName},
                       % check if append friend should be a in brackets
                       Rooms_ = dict:append(RoomID, [Friend], Rooms),
                       main_loop(ActiveUsers,Rooms_);
             error ->  print2("no user found named ", Friend),
                       main_loop(ActiveUsers,Rooms)
          end;

      %% --- Server Commands ---- %%
      {user_count} ->
          UserCount = length(fetch_keys(ActiveUsers)),
          io:format("There are current ~w active users ~n", [UserCount]),
          main_loop(ActiveUsers,Rooms);

      {quit} ->
           print("--- Server closed ---");

      _ -> print("no match in main loop"),
           main_loop(ActiveUsers,Rooms)
  end.


% Sends the message Msg to all users in the list users
% first as an erlnag message to the client node
% and the throught TCP to the client
send_msg_to([], _, _, _) -> ok;
send_msg_to([User|Users], ActiveUsers, RoomID, Msg) ->
  case dict:find(User, ActiveUsers) of
     {ok, [PID]} ->
         PID ! {msg, RoomID, Msg},
         send_msg_to(Users, ActiveUsers, RoomID, Msg);
      _ -> ok
  end.

% General function to send the erlang
% messege Msg to all client nodes in Users
notify_users([], _, _) -> ok;
notify_users([User|Users], ActiveUsers, Msg) ->
  case dict:find(User, ActiveUsers) of
     {ok, [PID]} ->
         PID ! Msg,
         notify_users(Users, ActiveUsers, Msg);
      _ -> ok%someoen in room was not found.
  end.


% this function spawns a new client node
start_client_node(LS,ServerPID) ->
    spawn(fun () -> client_node(LS,ServerPID) end).

% waits for a client to connect throught tcp
% if connection was sucessful it spawns a
% new client node and enters a recive loop
client_node(LS,ServerPID) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            start_client_node(LS, ServerPID),
            loop(S,ServerPID, undefined);
        _ -> start_client_node(LS, ServerPID)
    end.

% Recive loop for client node, it waits for
% masseges from both the client and the server
loop(S,ServerPID, User) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            handle_client_msg(S, Data, ServerPID),
            loop(S, ServerPID, User);
        {tcp_closed,S} ->
            %io:format("Socket ~w closed [~w]~n",[S,self()]),
            case User of
                undefined -> ok;
                _Defined -> ServerPID ! {logout, User}
            end;

        {register, UserName} -> loop(S, ServerPID, UserName);

        {chat_started, RoomID, Friend} ->
            send_client(S, io_lib:fwrite("{chat_started, ~p, ~p}.", [RoomID, Friend])),
            loop(S, ServerPID, User);

        {chat_dismissal, Friend} ->
            send_client(S, io_lib:fwrite("{chat_dismissal, ~p}.", [Friend])),
            loop(S, ServerPID, User);

        {chat_request, RoomID, Friend} ->
            send_client(S, "{chat_request,\"" ++ RoomID ++"\",\"" ++ Friend ++ "\"}."),
            loop(S, ServerPID, User);

        {msg, RoomID, Msg} ->
             send_client(S, "{msg,\"" ++ RoomID ++"\",\"" ++ Msg ++ "\"}."),
             loop(S, ServerPID, User);
        _ -> io:format("Unknown packet\n")
    end.

% When a client node recives and message from a client
% the message is sent here and parsed to erlang terms
% and then matched with the correct request
handle_client_msg(S, RawData, ServerPID) ->
    %io:format("Raw:\n~s~n", [RawData]),
    try
        {ok, Toks, _Line} = erl_scan:string(RawData, 1),
        {ok, Expr} = erl_parse:parse_term(Toks),

        case Expr of
            {login, UserName} -> ServerPID ! {register, UserName, self()},
                                self() ! {register, UserName},
                                send_client(S, ok);
            {msg, RoomID, Msg} -> io:format("Msg <~s>: ~s~n", [RoomID, Msg]), ServerPID ! {msg, RoomID, Msg, self()};
            {list_users} -> ServerPID ! {list_users, self()},
                            receive {Users} -> send_client(S, io_lib:fwrite("{list_users, ~p}.", [Users])) end;
            {logout, UserName} -> ServerPID ! {logout, UserName};
            {connect, UserName, Friend} ->  ServerPID ! {connect, UserName, Friend, self()};
            {start_group, Users, RoomID, UserName} -> ServerPID ! {start_group, Users, RoomID, UserName, self()};
            {invite, Friend, RoomID, UserName} -> ServerPID ! {invite, Friend, RoomID, UserName};
            _ -> io:format("Error on:\n~s", [RawData]), 'error'
        end
    catch
        _Class:_ ->
          send_client(S, "Something went wrong")
    end.

% sends a the massage Msg to the client
send_client(S, Msg) ->
  gen_tcp:send(S, io_lib:fwrite("~s", [Msg])).

print(Msg) ->
  io:format("~s~n",[Msg]).

print2(Msg1,Msg2) ->
  io:format("~p ~p~n",[Msg1,Msg2]).
