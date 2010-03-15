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

%% If we are at the end of request, return the request
receive_command(?END ++ T, L) -> {reverse(L), T};               
%% Work our way through the list, from H to T
receive_command([H|T], L)           -> receive_command(T, [H|L]);     
%% We went thru list with no "\r\n\r\n" so we need more.
receive_command([], _)              -> more.                          

%% Handles a request from the client.  The request has not been
%% validated yet.
handle_request(Request, Socket) ->
  case parse_request(Request) of
    {cache_set, Key, Value}    -> set_value(Key, Value);
    {cache_get, Key}           -> get_value(Key);
    {cache_del, Key}           -> delete_value(Key);
    {general_stats}            -> send_general_stats();
    {key_stats, Key}           -> send_key_stats(Key);
    {parse_error, Reason}      -> self() ! {parse_error, Reason}
  end,
  receive
    {set_ok, Key1} 
      -> gen_tcp:send(Socket, "OK SET " ++ Key1 ++ ?END);
    {get_ok, Value1}             
      -> gen_tcp:send(Socket, "OK GET " ++ Value1 ++ ?END);
    {get_not_found, Key1}
      -> gen_tcp:send(Socket, "OK KEY NOT FOUND " ++ Key1 ++ ?END);
    {delete_ok, Key1}            
      -> gen_tcp:send(Socket, "OK DELETE " ++ Key1 ++ ?END);
    {key_stats_ok, Key1, Stats}  
      -> gen_tcp:send(Socket, "OK KEY STATS " ++ Key1 ++ " " ++ Stats ++ ?END);
    {general_stats_ok, Stats}   
      -> gen_tcp:send(Socket, "OK GENERAL STATS "  ++ Stats ++ ?END);
    {parse_error, UnknownCommand}       
      -> gen_tcp:send(Socket, "ERR MALFORMED OR UNKNOWN COMMAND '" ++ UnknownCommand ++ "'" ++ ?END);
    {command_error, Reason1}     
      -> gen_tcp:send(Socket, "ERR " ++ Reason1 ++ ?END)
  end.

parse_request(Request) ->
  case Request of 
    "SET " ++ KeyValue ->
      case regexp:split(KeyValue, " ") of
        {ok, [Key, Value]} -> 
            {cache_set, Key, Value};
        _ -> 
            {parse_error, "MALFORMED SET COMMAND"}
      end;
    "GET " ++ Key ->
      {cache_get, Key};
    "DEL " ++ Key ->
      {cache_del, Key};
    "STATS" -> 
      {general_stats};
    "KEY STATS " ++ Key ->
      {key_stats, Key};
    _ ->
      {parse_error, Request}
  end.

set_value(Key, Value) ->
  case ets:delete(?TABLE, list_to_binary(Key)) of
    true 
      -> case ets:insert(?TABLE, {list_to_binary(Key), list_to_binary(Value)}) of
          true
            -> self() ! {set_ok, Key};
          _ 
            -> self() ! {command_error, "FAILED TO INSERT"}
          end;
     _
      -> self() ! {command_error, "COULD NOT CLEAR KEY BEFORE INSERT"}
  end.

get_value(Key) ->
  case ets:lookup(?TABLE, list_to_binary(Key)) of 
    [{_Key1, Value}]
      -> self() ! {get_ok, binary_to_list(Value)};
    [] 
      -> self() ! {get_not_found, Key};
    _
      -> self() ! {command_error, "ERROR WHILE GETTING VALUE"}
  end.

delete_value(Key) ->
  self() ! {delete_ok, Key}.

send_key_stats(Key) ->
  self() ! {key_stats_ok, Key, []}.

send_general_stats() ->
  self() ! {general_stats_ok, []}.
 
