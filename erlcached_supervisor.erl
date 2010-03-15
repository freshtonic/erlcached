-module(erlcached_supervisor).
-behaviour(supervisor).
-export([start/0, start_in_shell_for_testing/0, start_link/1, init/1]).

start() ->
  spawn(fun() -> 
          supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = [])
        end).

start_in_shell_for_testing() ->
  {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
  unlink(Pid).

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init([]) ->
  %% install personal error handler
  %%gen_event:swap_handler(alarm_handler, 
  %%                          {alarm_handler, swap},
  %%                          {erlcached_alarm_handler, xyz}),

  {ok, {{one_for_one, 3, 10},
    [{tag1,
       {erlcached_server, start, []},
       permanent,
       10000,
       worker,
       [erlcached_server]}]}}.
      
