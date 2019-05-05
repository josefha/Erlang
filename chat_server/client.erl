
-module(client).
-export([start/1, start/2]).

%-type position()      :: 'home' | ROOMID.

-define(HOME, home).
-define(MAX_HISTORY, 50).

-record(state, {server,pos, user, printer, chats, receiver, parser}).

start(Port) -> start("localhost", Port).
start(Host, Port) ->

  {ok, Server} = gen_tcp:connect(Host, Port,
                               [{active,true}, {packet,2}]),

  Printer = spawn_link(fun printer/0),
  %_Reciever = spawn_link(fun() -> receiver(Printer) end),
  clear(),
  header("LACPP-chat.\tA project by Alexis & Josef"),
  UserName = login(Server, Printer),

  Printer ! {menu, UserName},
  State = #state{server = Server, pos = ?HOME, user = UserName, printer = Printer, chats = dict:new(), receiver = self()},
  Parser = spawn_link(fun() -> input_parser(State) end),
  receiver(State#state{parser = Parser}).
  %input_parser(Server, UserName, Printer).



  login(Server, _Printer) ->
    Input = string:trim(io:get_line("Please enter your name:\n")),
    UserName = re:replace(Input, "\\s+", "_", [global,{return,list}]),
    ok = gen_tcp:send(Server, "{login, \""++ UserName ++"\"}."),
    receive
      {tcp, _Socket, Bin} ->
          case Bin == "ok" of
            true ->  UserName;
            false -> login(Server, _Printer)
          end
    end.


%%------------------------------------------------------------------------------
%%
%%    Actor: Input parser
%%
%%------------------------------------------------------------------------------

input_parser(#state{server=Server, user=UserName, printer=Printer, receiver=Receiver, pos=Pos} = State) ->
  Line = string:trim(io:get_line("Enter text: ")),

  case Line of
    "" -> io:format("\033[1A"), input_parser(State);
    _Else ->

      Printer ! {msg_sent},
      Args = string:tokens(Line, " "),
      case lists:nth(1,Args) of

        "connect" when Pos =:= ?HOME -> 
                  Friend = lists:nth(2,Args),
                  % Determine if room exist already first
                  Receiver ! {priv_connect, UserName ++ "-" ++ Friend},
                  New_pos =
                    receive
                      {new_pos, undefined} ->
                        gen_tcp:send(Server, "{connect,\"" ++ UserName ++ "\"" ++ ",\""++ Friend ++"\"}."),
                        receive
                          {new_pos, undefined} -> ?HOME;
                          {new_pos, New} -> New 
                        end;
                      {new_pos, Exist} -> Exist
                    end,

                  input_parser(State#state{pos = New_pos});


        "enter" when Pos =:= ?HOME ->
                  Room = lists:nth(2,Args),
                  Receiver ! {priv_enter, Room},
                  New_pos = 
                    receive
                      {new_pos, undefined} -> ?HOME;
                      {new_pos, Exist} -> Exist
                    end,
                  input_parser(State#state{pos = New_pos});

        "list_users"  when Pos =:= ?HOME ->
                  gen_tcp:send(Server, "{list_users}."),
                  input_parser(State);

        "list"  when Pos =:= ?HOME ->
                  io:format("list\n"), Receiver ! {priv_list}, input_parser(State);


        "quit"  when Pos =:= ?HOME ->
                  gen_tcp:send(Server, "{logout, \""++ UserName ++"\"}."),
                  %Receiver ! {terminate};
                  init:stop();

        "exit"  when Pos =/= ?HOME ->
                  Receiver ! {priv_leave},
                  Printer ! {menu, UserName},
                  input_parser(State#state{pos = ?HOME});

        _Msg    when Pos =/= ?HOME ->
                  gen_tcp:send(Server, io_lib:fwrite("{msg, ~p, ~p}.", [Pos, Line])),
                  input_parser(State);

        _Invalid ->
          %io:format("No valid input: ~p~n", [Line]),
          Printer ! {menu, UserName},
          input_parser(State)
      end
  end.




%%------------------------------------------------------------------------------
%%
%%    Actor: Receiver
%%
%%------------------------------------------------------------------------------

% Each message received need to produce a new state which will update
% the state for the next loop
receiver(#state{printer=Printer, pos=Position, user = UserName, chats=Chat}=State) ->
  New_State = receive

    {priv_list} ->
              Printer ! {list, "Converstations:", dict:fetch_keys(Chat)},
              timer:sleep(2000),
              Printer ! {menu, UserName},
              State;

    {priv_leave} ->
              State#state{pos = ?HOME};

    {priv_connect, Room} ->
              case dict:find(Room, Chat) of
                error ->
                  State#state.parser ! {new_pos, undefined},
                  State;
                {ok, History} -> 
                  State#state.parser ! {new_pos, Room},
                  Printer ! {start_conv, Room, History},
                  io:format("Room: ~p", [Room]),
                  State#state{pos = Room}
              end;

    {priv_enter, Room} ->
              case dict:find(Room, Chat) of
                error ->
                  State#state.parser ! {new_pos, undefined},
                  Printer ! {list, "", ["No such conversation available", "\t" ++ Room]},
                  timer:sleep(1500),
                  Printer ! {menu, UserName},
                  State;
                {ok, History} -> 
                  State#state.parser ! {new_pos, Room},
                  Printer ! {start_conv, Room, History},
                  io:format("Room: ~p", [Room]),
                  State#state{pos = Room}
              end;
    
    {terminate} ->
              exit(State#state.parser, normal),
              exit(Printer, normal),
              exit(normal),
              State;



    {tcp, _Socket, Bin} ->
          {ok, Toks, _Line} = erl_scan:string(Bin),
          {ok, Res} = erl_parse:parse_term(Toks),

          case Res of
            
            {list_users, List} ->
              Printer ! {list, "All users:", List},
              timer:sleep(2000),
              Printer ! {menu, UserName},
              State;

            {chat_started, RoomId, _To_Invited} ->
              State#state.parser ! {new_pos, RoomId},
              Printer ! {start_conv, RoomId, []},
              State#state{pos = RoomId, chats = dict:store(RoomId, [], Chat)};

            {chat_dismissal, Friend} ->
              State#state.parser ! {new_pos, undefined},
              Printer ! {list, "", ["No user with that name available", "\t" ++ Friend]},
              timer:sleep(1500),
              Printer ! {menu, UserName},
              State;

            {chat_request, RoomId, _From_Invite} ->
              State#state{chats = dict:store(RoomId, [], Chat)};

            % Message received when in the correct conversation
            {msg, RoomId, Msg} when Position =:= RoomId ->
              Printer ! {msg, RoomId, Msg},
              Dict = dict:store(RoomId,
                case dict:find(RoomId, Chat) of
                  {ok, History} ->[Msg|lists:sublist(History, ?MAX_HISTORY)];
                  error -> [Msg]
                end, Chat),
              State#state{chats = Dict};

            % Message received From other conversation than opened
            {msg, RoomId, Msg} ->
              Dict = dict:store(RoomId,
                case dict:find(RoomId, Chat) of
                  {ok, History} ->[Msg|lists:sublist(History, ?MAX_HISTORY)];
                  error -> [Msg]
                end, Chat),
              State#state{chats = Dict};

            _Not_implemented -> State
          end;
           
    _Invalid ->
      %io:format(": ~p~n", [_A]),
      State
  end,
  receiver(New_State).




%%------------------------------------------------------------------------------
%%
%%    Actor: Printer
%%
%%------------------------------------------------------------------------------

% ANSI escape codes:
% \033[<row>;<col>H   -   Place cursor at position, empty value means 0
% \033[s              -   Save current cursor position
% \033[u              -   Return to saved cursor position
% \033[<value>J       -   Clear screen, <value> signifies below-, above cursor or entire screen
% \033[K              -   Clear current row
% \033[<n>A           -   Move cursor n rows up


-define(CONV_START, 8).
-define(BOTTOM_DIVIDER, (Rows-4)).
-define(PRINT_SIZE, ?BOTTOM_DIVIDER - 2 - ?CONV_START).

-define(DIVIDER, lists:duplicate(Columns, $=)).
-define(PRINT(Lines), format_buffer(lists:reverse(lists:sublist(Lines, ?PRINT_SIZE)), ?PRINT_SIZE)).


printer() -> printer([]).
printer(Buffer) ->
  {ok, Columns} = io:columns(),
  {ok, Rows} = io:rows(),

  receive

    {list, Header, List} ->
      clear(),
      header(Header),
      io:format("\033[~p;H~s", [?CONV_START, format_buffer(List, ?PRINT_SIZE)]),
      io:format("\033[~p;H~s", [?BOTTOM_DIVIDER, ?DIVIDER]),
      cursor_position(),
      printer(Buffer);

    {start_conv, RoomId, Buffer_} ->
      clear(),
      header(room_header(RoomId)),
      io:format("\033[~p;H~s\033[~p;H~s", [?CONV_START, ?PRINT(Buffer_), ?BOTTOM_DIVIDER, ?DIVIDER]),
      cursor_position(),
      printer(Buffer_);

    {msg, RoomId, Msg} ->
      io:format("\033[s\033[~pA\033[1J", [4]),
      header(room_header(RoomId)),
      io:format("\033[~p;H~s", [?CONV_START, ?PRINT([Msg | Buffer])]),
      io:format("\033[u"),
      printer([Msg|lists:sublist(Buffer, ?MAX_HISTORY)]);

    {msg_sent} ->
      io:format("\033[2J\033[~p;H~s", [?BOTTOM_DIVIDER, ?DIVIDER]),
      cursor_position(),
      printer(Buffer);

    {menu, UserName} ->
      clear(),
      header("Welcome " ++ UserName ++ "!"),
      io:format("\033[6;H"),
      io:format( "
                  connect {Username}  -  Connect to a user \n
                  list                -  List all current conversations\n
                  enter {conv}        -  Enter a conversation\n
                  list_users          -  List all users online\n
                  quit                -  Quit program"),
      cursor_position(),
      printer([])
  end.

room_header(Room) ->
  Room ++ "\t-\ttype 'exit' to return to menu".

clear() ->
  io:format("\033[2J\033[;H").

header(Text) ->
  {ok, Columns} = io:columns(),
  io:format("\033[;H~s\n\033[K\n\033[K\t~s\033[900D\n\033[K\n\033[K~s\n\033[K", [?DIVIDER, Text,?DIVIDER]).


cursor_position() ->
  {ok, Rows} = io:rows(),
  io:format("\033[~p;H", [Rows-2]).

format_buffer([], _) -> "";
format_buffer(_, 0) -> "";
format_buffer([Msg | Buffer], N) ->
  "\033[K\t" ++ Msg ++ "\n" ++ format_buffer(Buffer, N-1).
