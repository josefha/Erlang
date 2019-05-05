%%%-------------------------------------------------------------------
%%% @author Martin & Eric <erlware-dev@googlegroups.com>
%%%  [http://www.erlware.org]
%%% @copyright 2008-2010 Erlware
%%% @doc RPC over TCP server. This module defines a server process that
%%%      listens for incoming TCP connections and allows the user to
%%%      execute RPC commands via that TCP stream.
%%% @end
%%%-------------------------------------------------------------------

-module(vector_server).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).

-import(string, [trim/1, to_integer/1]).
-import(lists, [droplast/1, append/2, zipwith/3, seq/2, sum/1, max/1]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @spec start_link() -> {ok, Pid}
%% @doc Calls `start_link(Port)' using the default port.
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% @doc Fetches the number of requests made to this server.
%% @spec get_count() -> {ok, Count}
%% where
%%  Count = integer()
%% @end
%%--------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port]) ->
    {ok, LSock} = gen_tcp:listen(Port, [{keepalive, true},{reuseaddr, true},{active, true}]),
    {ok, #state{port = Port, lsock = LSock}, 0}.

handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.


handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State};
handle_info({tcp_closed, _}, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_rpc(Socket, RawData) ->
    try
        {ok, Toks, _Line} = erl_scan:string(RawData ++ ".", 1),
        {ok, Expr} = erl_parse:parse_term(Toks),
        Result = eval(Expr),
        gen_tcp:send(Socket, io_lib:fwrite("Res: ~w~n", [Result]))
    catch
        _Class:_ ->
          gen_tcp:send(Socket, io_lib:fwrite("Res: ~p~n", ['error']))
    end.

eval(Expr) ->
  eval(0,Expr).

eval(Depth, Expr) ->
    case Depth > 100 of
      true -> 'error';
      false ->
        D = Depth + 1,
        case Expr of
            L when is_list(L), length(L) > 0, length(L) < 101 -> L;
            {add, Expr1, Expr2} -> add(eval(D,Expr1), eval(D,Expr2));
            {sub, Expr1, Expr2} -> sub(eval(D,Expr1), eval(D,Expr2));
            {dot, Expr1, Expr2} -> dot(eval(D,Expr1), eval(D,Expr2));
            {'mul', Value, Expr1} -> mul_(eval_int(Value), eval(D,Expr1));
            {'div', Value, Expr1} -> div_(eval_int(Value), eval(D,Expr1));
            _ -> 'error'
        end
end.

eval_int(Value) ->
  case Value of
    {norm_one, Expr} -> norm_one(eval(Expr));
    {norm_inf, Expr} -> norm_inf(eval(Expr));
    V when is_integer(V) -> V;
    _ -> error
  end.

mul_(Value,V1) ->
    V2 = [Value || _ <- lists:seq(1,length(V1))],
    lists:zipwith(fun(X, Y) -> X*Y end, V1, V2).

div_(Value,V1) ->
    V2 = [Value || _ <- lists:seq(1,length(V1))],
    lists:zipwith(fun(X, Y) -> X/Y end, V1, V2).

add(V1,V2) ->
    lists:zipwith(fun(X, Y) -> X+Y end, V1, V2).

sub(V1,V2) ->
    lists:zipwith(fun(X, Y) -> X-Y end, V1, V2).

dot(V1,V2) ->
    lists:zipwith(fun(X, Y) -> X*Y end, V1, V2).

norm_one(List) ->
    lists:sum(map(fun(X) -> abs(X) end, List)).

norm_inf(List) ->
    lists:max(map(fun(X) -> abs(X) end, List)).

map(_, []) -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

%% test
start_test() ->
    {ok, _} = vector_server:start_link(1055).
