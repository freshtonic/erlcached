
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

start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:call(?MODULE, stop).

init([]) -> {ok, []}.

%% Dummy implementation of handle_call.
handle_call(_, _From, State) ->
  {reply, ok, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Protocol implementation starts here

start_listening(Port) ->
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

