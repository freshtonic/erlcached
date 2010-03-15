-module(erlcached).
-export([start/1]).
-import(lists, [reverse/1]).
-define(TABLE, erlcached_table).
-define(END, "\r\n").

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
  %% fully-qualified function name to connect will invoke latest
  %% loaded code, enabling hot code swap :0)
  spawn(fun() -> erlcached:connect(Listen) end),
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
          loop(Socket, Request1);
        {Command, _Rest} ->
          try parse_and_execute(Socket, Request1) of 
          catch
            throw:Error -> 
              gen_tcp:send(Socket, "SERVER_ERROR " ++ Error ++ ?END)
          end
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

%% Parses a Memcached command and returns a tuple containing
%% the function to invoke and its arguments.  If there was an error
%% parsing the command or its arguments, a tuple of the form 
%% {error, Description} will be returned.
%% The specs for the protocol can be found here:
%% http://cvs.danga.com/browse.cgi/wcmtools/memcached/doc/protocol.txt?rev=HEAD&content-type=text/plain
parse_command("set " ++ T)
  -> make_storage_command(command_set, parse_rest_of_storage_command(T));
parse_command("add " ++ T)
  -> make_storage_command(command_add, parse_rest_of_storage_command(T));
parse_command("replace " ++ T)
  -> make_storage_command(command_replace, parse_rest_of_storage_command(T));
parse_command("delete " ++ T)
  -> make_deletion_command(parse_rest_of_deletion_command(T));
parse_command("get " ++ T)
  -> make_retrieval_command(parse_rest_of_retrieval_command(T));
parse_command("stats" ++ T) -> 
  case T of
    [] -> make_stats_command();
    " " ++ Args -> make_stats_command(parse_rest_of_stats_command(T))
  end;
parse_command("incr " ++ T) ->
  make_incr_or_decr_command(command_incr, T);
parse_command("decr " ++ T) ->
  make_incr_or_decr_command(command_decr, T);
parse_command(BadCommand) 
  -> {error, BadCommand}.


parse_rest_of_storage_command(Rest) ->
  case regexp:split(Rest, " ") of
    [Key, Flags, Exptime, Bytes] -> {Key, Flags, Exptime, Bytes};
    _ -> error
  end.

parse_rest_of_deletion_command(Rest) ->
  case regexp:split(Rest, " ") of
    [Key, Time] -> {Key, Time}
  end.

parse_rest_of_retrieval_command(Rest) ->
  case Rest of 
    " " ++ Key -> {Key};
    _ -> error
  end.
      
parse_rest_of_stats_command(Rest) ->
  nothing.

make_storage_command(Command, {Key, Flags, Exptime, Bytes}) ->
  CommandFunc = case Command of 
                  command_set -> fun set_value/4;
                  command_add -> fun add_value/4;
                  command_replace -> fun replace_value/4
                end,
  {CommandFunc, Key, list_to_integer(Flags), list_to_integer(Exptime), list_to_integer(Bytes)}.

make_deletion_command({Key, Time}) ->
  {fun delete/2, Key, list_to_integer(Time)}.

make_retrieval_command(Keys) ->
  {fun get_value/1, Keys}.

make_incr_or_decr_command(Command, Rest) ->
  CommandFunc = case Command of
                  command_incr -> fun incr_value/2;
                  command_decr -> fun decr_value/2
                end,
  case regexp:split(Rest) of
    [Key, Value] -> 
      {CommandFunc, Key, list_to_integer(Value)};
    _ -> error
  end.

make_stats_command() ->
  {fun stats/1}.

make_stats_command(Args) ->
 {error}. 

%% The following functions are those that manipulate or query the cache.

set_value(Socket, Key, Flags, Exptime, Bytes) ->
  ets:delete(?TABLE, Key), %% clear any existing value at the key.
  ets:insert(?TABLE, {Key, Exptime, Flags, Bytes}).

get_value(Socket, Keys) ->
  {no}.

add_value(Socket, Key, Flags, Exptime, Bytes) ->
  case ets:lookup(?TABLE, Key) of 
    [] -> set_value(Key, Flags, Exptime, Bytes),
        ok;
    _ -> {error, "key already exists"}
  end.

replace_value(Socket, Key, Flags, Exptime, Bytes) ->
  case ets:lookup(?TABLE, Key) of
    [] -> {error, "key does not exist"};
    _ -> set_value(Key, Flags, Exptime, Bytes),
        ok
  end.

delete(Socket, Key, Time) ->
  spawn(fun() -> delayed_delete(Key, Time) end).

delayed_delete(Key, Time) ->
  receive
    Any -> Any
  after Time ->
    ets:delete(?TABLE, Time)
  end.

incr_value(Socket, Key, Value) ->
  {ok}.

decr_value(Socket, Key, Value) ->
  {ok}.

stats(Socket, Key) ->
  {ok}.

%% Utility functions

key_exists(Key) ->
  case ets:lookup(?TABLE, Key) of
    [] -> false
    _ -> true
  end.


