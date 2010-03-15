-module(erlcached_launch).
-export([start/0, start_with_appmon/0]).

start() -> 
  application:start(erlcached).

start_with_appmon() ->
  start(),
  appmon:start().
