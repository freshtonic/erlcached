
%% This module implements the Memcached ASCII protocol.
%% It aims for 100% compatibility with existing Memcached clients.

-module(erlcached_memcached_ascii_protocol).
-behaviour(gen_server).
-export([start/0]).
%% gen_server callbacks
-export([
  init/1, 
  handle_call/3, 
  handle_cast/2, 
  handle_info/2, 
  terminate/2, 
  code_change/3]).
-compile(export_all).

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [{port, get_port()}], []).
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

%% Module implementation begins here

connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> connect(Listen) end),
  loop(Socket, []).
 
%% Loops over a socket until a complete request has been received from a
%% client. Once  a complete request  has been received, it  is validated
%% and processed. After processing of a request, the socket is closed.
%% TODO: timeout waiting for data.
loop(Socket, Request) ->
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Data} ->
      Request1 = list_to_binary([Request, Data]),
      {Command, Rest} =  read_line(Socket, Request1, <<>>),
      case parse_command(Command) of
        {cmd_set, {Key, Flags, Exptime, ByteCount}} ->
          {Blob, Rest1} = read_blob(Socket, ByteCount, Rest),
          {_, Rest2} = read_line(Socket, Rest1, <<>>),
          case erlcached_server:cache_set(Key, Flags, Exptime, Blob) of
             ok -> reply(Socket, "STORED\r\n");
             Error -> handle_error(Socket, Error)
          end,
          io:format("next command~n"),
          loop(Socket, Rest2);
        {cmd_get, Keys} ->
          try handle_get(Socket, Keys)
          catch
            _:_ -> handle_error(Socket, {server_error, ""}) %% TODO: extract the reason
          end;
        {bad_command, _BadCommand}->
          handle_error(Socket, {client_error, "Bad command"})
      end,
      loop(Socket, Rest);
    {tcp_closed, Socket} ->
      io:format("server socket closed~n");
    _Any ->
      io:format("received unknown message~n")
  end.

handle_get(Socket, Keys) ->
  Values = erlcached_server:cache_get(Keys),
  send_values(Socket, Values).

%% Gets the port that we listen on from the application
%% configuration.
get_port() ->
  case application:get_env(erlcached, ascii_protocol_port) of
    {ok, Port} when is_integer(Port), Port > 0, Port < 65535 ->
      Port;
    Bad -> exit({bad_config, {erlcached, {ascii_protocol_port, Bad}}})
  end.

%% Reads a single line of text from the socket.  Returns the new line (without the
%% \r\n) and the rest of the binary data following the new line.
read_line(_Socket, "\r\n" ++ Rest, Line) -> 
  {lists:reverse(Line), list_to_binary(Rest)};               
read_line(Socket, Data, Rest) when is_binary(Data), is_binary(Rest) -> 
  read_line(Socket, binary_to_list(Data), binary_to_list(Rest));
read_line(Socket, [H|T], L) -> 
  read_line(Socket, T, [H|L]);     
read_line(Socket, [], Rest) -> 
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket, Data} ->
      read_line(Socket, binary_to_list(Data), Rest)
  end.

%% Parses a command and returns a tuple identifying the command
%% and its arguments.
parse_command("set " ++ Rest) ->
  parse_storage_command(cmd_set, Rest);
parse_command("add " ++ Rest) ->
  parse_storage_command(cmd_add, Rest);
parse_command("replace " ++ Rest) ->
  parse_storage_command(cmd_replace, Rest);
parse_command("get " ++ Rest) ->
  {ok, Keys} = regexp:split(Rest, " "),
  {cmd_get, [{Key} || Key <- Keys]};
parse_command(BadCommand) ->
  {bad_command, BadCommand}.


parse_storage_command(Command, Rest) ->
  io:format("parse_storage_command~n"),
  case regexp:split(Rest, " ") of
    {ok, [Key, Flags, Exptime, ByteCount]} ->      
      try {Command, {Key, list_to_integer(Flags), list_to_integer(Exptime), list_to_integer(ByteCount)}} 
      catch 
        _:_ -> {bad_command, "set " ++ Rest}
      end;
    _ -> {bad_command, "set " ++ Rest}
  end.

reply(Socket, TextOrData) when is_list(TextOrData) ->
  reply(Socket, list_to_binary(TextOrData));
reply(Socket, Data) ->
  gen_tcp:send(Socket, Data).

%% Replies with values found with the get command
send_values(Socket, [{Key, {Flags, _, Data}}|Rest]) ->
  reply(Socket, "VALUE " ++ Key ++ " " ++ integer_to_list(Flags) ++ " "  ++ integer_to_list(size(Data)) ++ "\r\n"),
  reply(Socket, Data),
  reply(Socket, "\r\n"),
  send_values(Socket, Rest);
send_values(Socket, [{_, not_found}|Rest]) ->
  send_values(Socket, Rest);
send_values(Socket, []) ->
  reply(Socket, "END\r\n").
  

handle_error(Socket, Error) ->
  case Error of 
    {server_error, Reason} -> reply(Socket, "SERVER_ERROR " ++ Reason ++ "\r\n");
    {client_error, Reason} -> reply(Socket, "CLIENT_ERROR " ++ Reason ++ "\r\n");
    error -> reply(Socket, "ERROR\r\n")
  end.


%% Reads a blob of binary data (of a predetermined size) from the socket.
%% Returns a tuple of the form {Blob, Rest} where Rest is the last chunk 
%% of the data read from the socket beyond the blob.
read_blob(_Socket, ByteCount, Data) when size(Data) >= ByteCount ->
  io:format("read_blob1~n"),
  split_binary(Data, ByteCount);
read_blob(Socket, ByteCount, Data) ->
  io:format("read_blob2~n"),
  inet:setopts(Socket, [{active, once}]),
  receive
    {tcp, Socket1, Data1} -> 
      read_blob(Socket1, ByteCount, list_to_binary([Data, Data1]))
  end.
