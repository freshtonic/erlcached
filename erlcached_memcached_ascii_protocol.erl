
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

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [{port, 9898}], []).
stop() -> gen_server:call(?MODULE, stop).
init([{port, Port}]) -> {ok, Port}.

%% Public API.
protocol_start_listening() -> 
  gen_server:call(?MODULE, {start_listening}).

%% gen_server callbacks
handle_call({start_listening}, _From, Port) ->
  {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, once}]),
  spawn(fun() -> connect(Listener) end),
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
  receive
    {tcp, Socket, Data} ->
      inet:setopts(Socket, [{active, once}]),
      Request1 = Request ++ binary_to_list(Data),
      case receive_first_line_of_command(Request1, []) of
        more ->
          loop(Socket, Request1);
        {Command, _Rest} ->
          gen_tcp:send(Socket, "Received command: " ++ Command),
          gen_tcp:close(Socket)
      end;
    {tcp_closed, Socket} ->
      io:format("server socket closed~n");
    _Any ->
      io:format("received unknown message~n")
  end.

%% Gets the port that we listen on from the application
%% configuration.
get_port() ->
  case application:get_env(erlcached_memcached_ascii_protocol, port) of
    {ok, Port} when is_integer(Port), Port > 0, Port < 65535 ->
      Port;
    Bad -> exit({bad_config, {erlcached_memcached_ascii_protocol, {port, Bad}}})
  end.


%% If we are at the end of request, return the request
receive_first_line_of_command("\r\n" ++ T, L) -> {lists:reverse(L), T};               
%% Work our way through the list, from H to T
receive_first_line_of_command([H|T], L)           -> receive_first_line_of_command(T, [H|L]);     
%% We went thru list with no "\r\n\r\n" so we need more.
receive_first_line_of_command([], _)              -> more.      
