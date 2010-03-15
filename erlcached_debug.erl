-module(erlcached_debug).
-export([debug_ascii_protocol/0, debug_server/0]).
-compile(export_all).


%% Useful utility function to enable debugging of this module.
%% It probably shouldn't be in here, but I haven't figured out
%% a way of doing it from the Makefile because the Erlang compiler
%% doesn't seem to support passing debug compilation flags.
debug_module(Module) ->
  c:c(Module, [debug_info]),
  i:ii(Module),
  i:iaa([init]).

debug_ascii_protocol() ->
  debug_module(erlcached_memcached_ascii_protocol).

debug_server() ->
  debug_module(erlcached_server).
