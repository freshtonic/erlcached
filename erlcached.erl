-module(erlcached).
-export([start/1]).
-import(lists, [reverse/1]).
-define(TABLE, erlcached_table).
-define(END, "\r\n\r\n").

%% Starts  erlcached listening  on Port  The server  listens using  the.
%% 'Hybrid Approach' (neither blocking  nor non-blocking) This function.
%% returns the Pid of root process that invokes gen_tcp:listen.
start(Port) ->
  init_cache(),
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, 
    {reuseaddr, true}, {active, once}]),
  spawn(fun() -> connect(Listen) end).

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> connect(Listen) end),
  loop(Socket, []).
 
%% Loops over a socket until a complete request has been received from a
%% client. Once  a complete request  has been received, it  is validated
%% and processed. After processing of a request, the socket is closed.
%% TODO: timeout waiting for data.
loop(Socket, Request) ->
  receive
    {tcp, Socket, Data} ->
      inet:setopts(Socket, [{active, once}]),
      Request1 = Request ++ binary_to_list(Data),
      case receive_command(Request1, []) of
        more ->
          %% the request is incomplete, we need more data
          loop(Socket, Request1);
        {CompleteRequest, _Rest} ->
          %% header is complete
          handle_request(CompleteRequest, Socket)
      end;
    {tcp_closed, Socket} ->
      io:format("server socket closed~n");
    _Any ->
      io:format("received unknown message~n")
  end.

%% Creats the ETS table that will be out backing store for the erlcached.
init_cache() ->
  ets:new(?TABLE, [set, public, named_table]).

%% Receives  a single  command. It  doesn't validate  the command,  just
%% reads data until we have read two blank lines. TODO: limit the amount
%% of data we consume here?
receive_command(?END ++ T, L) -> {reverse(L), T};               %% If we are at the end of request, return the request
receive_command([H|T], L)           -> receive_command(T, [H|L]);     %% Work our way through the list, from H to T
receive_command([], _)              -> more.                          %% We went thru list with no "\r\n\r\n" so we need more.

%% Handles a request from the client.  The request has not been
%% validated yet.
handle_request(Request, Socket) ->
  case parse_request(Request) of
    {cache_set, Key, Value, Expires}    -> set_value(Key, Value, Expires};
    {cache_get, Key}                    -> get_value(Key);
    {cache_delete, Key}                 -> delete_value(Key);
    {cache_stats, Key}                  -> send_key_stats(Key);
    {cache_stats, general}              -> send_general_stats();
    {parse_error, Reason}               -> self ! {parse_error, Reason}
  end;
  self ! close_connection,
  receive
    {set_ok, Key}               -> gen_tcp:send(Socket, "OK SET " ++ Key ++ ?END);
    {get_ok, Value}             -> gen_tcp:send(Socket, "OK GET " ++ Value ++ ?END);
    {delete_ok, Key}            -> gen_tcp:send(Socket, "OK DELETE " ++ Key ++ ?END);
    {key_stats_ok, Key, Stats}  -> gen_tcp:send(Socket, "OK KEY STATS " ++ Key ++ " " ++ Stats ++ ?END);
    {general_stats_ok, Stats}   -> gen_tcp:send(Socket, "OK GENERAL STATS " ++ Key ++ " " ++ Stats ++ ?END);
    {parse_error, Reason}       -> gen_tcp:send(Socket, "ERR MALFORMED OR UNKNOWN COMMAND: " ++ Reason ++ ?END);
    {command_error, Reason}     -> gen_tcp:send(Socket, "ERR " ++ Reason ++ ?END)
  end.

set_value(Key, Value, _Expires) ->
  case ets:insert(?TABLE, {Key, Value}) of
    true -> self ! {cache_set, Key, Value, _Expires},
    _ -> self ! {command_error, "COULD NOT INSERT KEY " ++ Key}
  end.

get_value(Key) ->
  case ets:lookup(?TABLE, Key) of
    [] -> 
  end.

