%% This module implements the Memcached ASCII protocol.
%% It aims for 100% compatibility with existing Memcached clients.
-module(erlcached_memcached_ascii_protocol).
-behaviour(gen_server).
-export([start/0]).
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3]).
-compile(export_all).
-define(STORAGE_COMMAND_TIMEOUT_MILLIS, infinity).

start() -> 
  gen_server:start_link
    ({local, ?MODULE}, ?MODULE, [{port, get_port()}], []).

stop() -> gen_server:call(?MODULE, stop).

init([{port, Port}]) -> 
  {ok, Listener} = gen_tcp:listen(Port, 
    [binary, {packet, 0}, {reuseaddr, true}, {active, once}]),
  spawn(fun() -> connect(Listener) end),
  {ok, Port}.

%% gen_server callbacks
handle_call(_, _From, Port) ->
  {reply, ok, Port}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> connect(Listen) end),
  erlcached_memcached_ascii_protocol:loop(Socket, <<>>).

loop(Socket, Buffer) ->
  loop(Socket, 
    case try_parse_command(Buffer) of
      {cmd_get, {Keys}, Rest}                           -> 
        send_values(Socket, erlcached_server:cache_get(Keys)), Rest;
      {cmd_set, {Key, Flags, Expire, Bytes}, Rest}      -> 
        handle_storage_command(Socket, fun erlcached_server:cache_set/4, 
        Key, Flags, Expire, Bytes), Rest;
      {cmd_add, {Key, Flags, Expire, Bytes}, Rest}      -> 
        handle_storage_command(Socket, fun erlcached_server:cache_add/4, 
        Key, Flags, Expire, Bytes), Rest;
      {cmd_replace, {Key, Flags, Expire, Bytes}, Rest}  -> 
        handle_storage_command(Socket, fun erlcached_server:cache_replace/4, 
        Key, Flags, Expire, Bytes), Rest;
      {cmd_delete, {Key}, Rest}                         -> 
        handle_delete_command(Socket, Key), Rest;
      {cmd_incr, {Key}, Rest}                           ->
        handle_incr_or_decr_command(Socket, 
        fun erlcached_server:cache_incr/1, Key), Rest;
      {cmd_decr, {Key}, Rest}                           ->
        handle_incr_or_decr_command(Socket, 
        fun erlcached_server:cache_decr/1, Key), Rest;
      {parse_error, RestAfterLastNewline}               -> 
        reply(Socket, "CLIENT_ERROR bad command line format\r\n"), 
        RestAfterLastNewline
    end).

%% Grab some more  data, but only wait  for a maximum amount  of time in
%% millis or an  infinite amount of time if specified.  We will wait for
%% an infinite  amount of time  between commands  sent by the  client. A
%% large storage command sent by a client may be received as a number of
%% fragments that consume a significant amount of memory. If an error in
%% the client  means that it  fails to  close the connection,  the bytes
%% composing the partial command will be consuming memory on the server.
%% Therefore, during  receipt of storage  commands we use more()  with a
%% finite  timeout. If  more()  times  out, we  kill  the connection  by
%% causing this process to exit.
more(TimeOut) ->
  receive
    {tcp, _Socket, Chunk} -> Chunk;
    {tcp_closed, _Socket} -> 
      io:format("socket closed!~n"),
      exit(socket_closed)
  after TimeOut ->
    io:format("client timed out! " ++ integer_to_list(TimeOut) ++ "~n"),
    exit(client_timed_out)
  end.

%% Tries to parse  a command, and returns a tuple  containing the result
%% of the parse. If the data cannot be parsed (for whatever reason), the
%% tuple {parse_error,  <<...>>} is  returned, where  the binary  is the
%% remaining data  after the  last newline (\r\n)  from the  binary that
%% could not be parsed. Parsing can be re-attempted from that point.
try_parse_command(Rest) ->
  try parse_command(Rest)
  catch
    throw:_ -> {parse_error, rest_after_last_newline(Rest)}
  end.


parse_command(<<"set ", Rest/binary>>) ->
  parse_storage_command(cmd_set, Rest);
parse_command(<<"add ", Rest/binary>>) ->
  parse_storage_command(cmd_set, Rest);
parse_command(<<"replace ", Rest/binary>>) ->
  parse_storage_command(cmd_set, Rest);
parse_command(<<>>) ->
  parse_command(more(infinity)).


parse_storage_command(Command, Rest) ->
  io:format("parse_storage_command~n"), 
  {{Key, Flags, Expire, ByteCount}, Rest1} 
    = parse_storage_command_args(Rest),
  {Bytes, Rest2} =  parse_storage_command_blob(ByteCount, Rest1),
  {Command, {Key, Flags, Expire, Bytes}, Rest2}.


parse_storage_command_args(Rest) ->
  io:format("parse_storage_command_args~n"),
  case index_of("\r\n", 0, Rest) of
    -1 -> 
      %% TODO: max length of args must be fixed.  
      %% If newline not found in time, we bail.
      parse_storage_command_args(list_to_binary
        ([Rest, more(?STORAGE_COMMAND_TIMEOUT_MILLIS)]));
    N ->  
      <<String:N/binary, "\r\n", Rest1/binary>> = Rest,
      {ok, [Key, Arg2, Arg3, Arg4]} 
        = regexp:split(binary_to_list(String), " "),
      [Flags, Expire, ByteCount] 
        = [list_to_integer(Arg) || Arg <- [Arg2, Arg3, Arg4]],
      {{Key, Flags, Expire, ByteCount}, Rest1}
  end.


parse_storage_command_blob(ByteCount, Rest) ->
  case size(Rest) >= (ByteCount + 2) of
    true -> 
      <<Bytes:ByteCount/binary, "\r\n", Rest1/binary>> = Rest,
      {Bytes, Rest1};
    _ -> 
      parse_storage_command_blob(ByteCount, 
        list_to_binary([Rest, more(?STORAGE_COMMAND_TIMEOUT_MILLIS)]))
  end.

%% Returns the index of the first occurence of a string within a binary
%% or -1 if it cannot be found.
index_of(_, _, <<>>) -> -1;
index_of(String, Index, Bin) ->
  case Bin of
    <<String, _/binary>> ->  Index;
    _  -> 
        <<_:8, Rest/binary>> = Bin,
        index_of(String, Index + 1, Rest)
  end.

rest_after_last_newline(Bin) ->
  list_to_binary(lists:reverse(raln(lists:reverse(binary_to_list(Bin))))).

raln([]) -> [];
raln(List) ->
  case List of
    ["\n\r"|Rest] -> Rest;
    [_|Rest] -> raln(Rest)
  end.

%% Gets the port that we listen on from the application
%% configuration.
get_port() ->
  case application:get_env(erlcached, ascii_protocol_port) of
    {ok, Port} when is_integer(Port), Port > 0, Port < 65535 ->
      Port;
    Bad -> exit({bad_config, {erlcached, {ascii_protocol_port, Bad}}})
  end.

%% Replies with values found with the get command
send_values(Socket, [{Key, {Flags, _, Data}}|Rest]) ->
  reply(Socket, "VALUE " ++ Key ++ " " 
    ++ integer_to_list(Flags) ++ " "  
    ++ integer_to_list(size(Data)) ++ "\r\n"),
  reply(Socket, Data),
  reply(Socket, "\r\n"),
  send_values(Socket, Rest);
send_values(Socket, [{_, not_found}|Rest]) ->
  send_values(Socket, Rest);
send_values(Socket, []) ->
  reply(Socket, "END\r\n").

handle_storage_command(Socket, StorageCommand, Key, Flags, Expire, Bytes) ->
  case StorageCommand(Key, Flags, Expire, Bytes) of
      ok -> reply(Socket, "STORED\r\n");
      Error -> reply(Socket, "SERVER_ERROR " ++ Error ++ "\r\n")
  end.

handle_delete_command(Socket, Key) ->
  case erlcached_server:cache_set(Key) of
    ok -> reply(Socket, "DELETED\r\n");
    error_key_not_found -> reply(Socket, "NOT_FOUND\r\n")
  end.

handle_incr_or_decr_command(Socket, Fun, Key) ->
  case Fun(Key) of
    {ok, NewValue} -> reply(Socket, integer_to_list(NewValue) ++ "\r\n");
    error_key_does_not_exist -> reply(Socket, "NOT_FOUND\r\n")
  end.

reply(Socket, TextOrData) when is_list(TextOrData) ->
  reply(Socket, list_to_binary(TextOrData));
reply(Socket, Data) ->
  gen_tcp:send(Socket, Data).




