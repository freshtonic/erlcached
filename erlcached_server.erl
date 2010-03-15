-module(erlcached_server).
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

%% The functions starting with 'cache_' represent the public interface
%% to the cache.
cache_set(Key, Flags, Expire, Bytes) -> 
  gen_server:call(?MODULE, {cache_set, Key, Flags, Expire, Bytes}).
cache_add(Key, Flags, Expire, Bytes) -> 
  gen_server:call(?MODULE, {cache_add, Key, Flags, Expire, Bytes}).
cache_replace(Key, Flags, Expire, Bytes) -> 
  gen_server:call(?MODULE, {cache_replace, Key, Flags, Expire, Bytes}).
cache_get(Key) when is_tuple(Key) -> 
  gen_server:call(?MODULE, {cache_get, [Key]});
cache_get(Keys) when is_list(Keys) ->
  gen_server:call(?MODULE, {cache_get, Keys}).
cache_delete(Key) -> 
  gen_server:call(?MODULE, {cache_delete, Key}).
cache_incr(Key) -> 
  gen_server:call(?MODULE, {cache_incr, Key}).
cache_decr(Key) -> 
  gen_server:call(?MODULE, {cache_decr, Key}).
cache_stats(Key) -> 
  gen_server:call(?MODULE, {cache_stats, Key}).
cache_stats() ->
  gen_server:call(?MODULE, {cache_stats}).

init([]) -> {ok, ets:new(?MODULE, [])}.

%% gen_server required callbacks:

handle_call({cache_set, Key, Flags, Expire, Bytes}, _From, Tab) ->
  clear_key(Tab, Key),
  set_key(Tab, Key, Flags, Expire, Bytes),
  {reply, ok, Tab};

handle_call({cache_add, Key, Flags, Expire, Bytes}, _From, Tab) ->
  case is_key_set(Tab, Key) of 
    true -> {reply, {error_key_already_set, Key}, Tab};
    _ -> set_key(Tab, Key, Flags, Expire, Bytes),
        {reply, ok, Tab}
  end;

handle_call({cache_replace, Key, Flags, Expire, Bytes}, _From, Tab) ->
  case is_key_set(Tab, Key) of
    true -> set_key(Tab, Key, Flags, Expire, Bytes),
            {reply, ok, Tab};
    _ -> {reply, {error_key_not_already_set, Key}, Tab}
  end;

handle_call({cache_get, Keys}, _From, Tab) ->
  {reply, [get_value(Tab, Key) || {Key} <- Keys], Tab}.



handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Helper functions

%% TODO: implement RAM limit for cache, and eviction policy

clear_key(Tab, Key) when is_list(Key) ->
  ets:delete(Tab, Key).

set_key(Tab, Key, Flags, Expire, Bytes) when 
  is_list(Key), 
  is_integer(Flags), 
  is_integer(Expire), 
  is_binary(Bytes) ->
  ets:insert(Tab, {Key, Flags, Expire, Bytes}).
  
is_key_set(Tab, Key) ->
  case length(ets:lookup(Tab, Key)) of 
    0 -> false;
    _ -> true
  end.

get_value(Tab, Key) when is_list(Key) ->
  case ets:lookup(Tab, Key) of
    [{_Key1, Flags, Expire, Bytes}] -> {Key, {Flags, Expire, Bytes}};
    [] -> {Key, not_found}
  end.

