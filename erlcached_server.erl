
%% This module is the backend of erlcached, i.e. the cache itself.
%% It implements the operations that can be performed on the cache
%% and keeps track of various cache statistics.  The cache itself
%% is an ETS table.  There is a second table that is used for
%% storing metadata and configuration information.

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

start() -> 
  io:format("erlcached_server:start()~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> 
  io:format("erlcached_server:stop()~n"),
  gen_server:call(?MODULE, stop).

%% The functions starting with 'cache_' represent the exported interface
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
cache_delete(Key, TimeSeconds) -> 
  gen_server:call(?MODULE, {cache_delete, Key, TimeSeconds}).
cache_delete(Key) ->
  gen_server:call(?MODULE, {cache_delete, Key, 0}).
cache_incr(Key, Amount) -> 
  gen_server:call(?MODULE, {cache_incr, Key, Amount}).
cache_decr(Key, Amount) -> 
  gen_server:call(?MODULE, {cache_decr, Key, Amount}).
cache_stats(Key) -> 
  gen_server:call(?MODULE, {cache_stats, Key}).
cache_stats() ->
  gen_server:call(?MODULE, {cache_stats}).
die() ->
  gen_server:call(?MODULE, {die}).

%% Initialises the module.  cache_table is the cache itself.
%% metadata_table stores the metadata about the cache table 
%% and various runtime statistics and state.
init([]) -> 
  io:format("erlcached_server:init()~n"),
  {ok, {ets:new(cache_table, [set, public]), 
                  ets:new(meta_data_table, [set, public])}}.

%% gen_server required callbacks:

handle_call({cache_set, Key, Flags, Expire, Bytes}, _From, State) ->
  Tab = get_cache(State),
  clear_key(Tab, Key),
  set_key(Tab, Key, Flags, Expire, Bytes),
  {reply, ok, State};

handle_call({cache_add, Key, Flags, Expire, Bytes}, _From, State) ->
  Tab = get_cache(State),
  case is_key_set(Tab, Key) of 
    true -> {reply, {error_key_already_set, Key}, State};
    _ -> set_key(Tab, Key, Flags, Expire, Bytes),
        {reply, ok, State}
  end;

handle_call({cache_replace, Key, Flags, Expire, Bytes}, _From, State) ->
  Tab = get_cache(State),
  case is_key_set(Tab, Key) of
    true -> set_key(Tab, Key, Flags, Expire, Bytes),
            {reply, ok, State};
    _ -> {reply, {error_key_not_already_set, Key}, State}
  end;

handle_call({cache_get, Keys}, _From, State) ->
  Tab = get_cache(State),
  {reply, [get_value(Tab, Key) || {Key} <- Keys], State};

handle_call({cache_delete, Key, TimeSeconds}, _From, State) ->
  Tab = get_cache(State),
  case is_key_set(Tab, Key) of
    true -> spawn(fun() -> 
                    receive
                    after TimeSeconds * 1000 ->
                      ets:delete(Tab, Key)
                    end
                  end),
        {reply, ok, State};
    false -> {reply, error_key_not_set, State}
  end;

handle_call({cache_incr, Key, Amount}, _From, State) ->
  Tab = get_cache(State),
  case incr(Tab, Key, Amount) of
    {ok, NewAmount} -> {reply, {Key, NewAmount}, State};
    error_key_does_not_exist -> {reply, {error, key_does_not_exist, Key}, State}
  end;

handle_call({cache_decr, Key, Amount}, _From, State) ->
  Tab = get_cache(State),
  case decr(Tab, Key, Amount) of
    {ok, NewAmount} -> {reply, {Key, NewAmount}, State};
    error_key_does_not_exist -> {reply, {error, key_does_not_exist, Key}, State}
  end;

handle_call({cache_stats}, _From, State) ->
  {reply, [ {"pid", os:getpid()}, 
            {"uptime"}, 
            {"time"}, 
            {"version"}, 
            {"rusage_user"}, 
            {"rusage_system"}, 
            {"curr_items"}, 
            {"total_items"},
            {"bytes"},
            {"curr_connections"},
            {"total_connections"},
            {"connection_structures"},
            {"cmd_get"},
            {"cmd_set"},
            {"get_hits"},
            {"get_misses"},
            {"evictions"},
            {"bytes_read"},
            {"bytes_written"},
            {"limit_maxbytes"}
          ], 
            State};

handle_call({die}, _From, _State) ->
  throw(die_horribly).

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

get_cache(State) ->
  {CacheTable, _MetaDataTable} = State, 
  CacheTable.

get_metadata(State) ->
  {_CacheTable, MetadataTable} = State,
  MetadataTable.

clear_key(Tab, Key) when is_list(Key) ->
  ets:delete(Tab, Key).

set_key(Tab, Key, Flags, Expire, Bytes) when 
  is_list(Key), 
  is_integer(Flags), 
  is_integer(Expire), 
  is_binary(Bytes) or is_integer(Bytes) ->
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

incr(Tab, Key, Amount) when is_integer(Amount), Amount >= 0 ->
  case get_value(Tab, Key) of
    {_Key, {Flags, Expire, Bytes}} -> 
      case is_integer(Bytes) of
        true -> NewAmount = Bytes + Amount, 
                set_key(Tab, Key, Flags, Expire, NewAmount),
                {ok, NewAmount};
        false -> set_key(Tab, Key, Flags, Expire, Amount),
                {ok, Amount}
      end;
    _ ->
        error_key_does_not_exist
  end;

incr(_, _, _) ->
  {error_amount_must_be_a_positive_integer}.

decr(Tab, Key, Amount) when is_integer(Amount), Amount >= 0 ->
  case get_value(Tab, Key) of
    {_Key, {Flags, Expire, Bytes}} -> 
      if 
        is_integer(Bytes) and ((Bytes - Amount) >= 0) ->
          NewAmount = Bytes - Amount, 
          set_key(Tab, Key, Flags, Expire, NewAmount),
          {ok, NewAmount};
        true -> set_key(Tab, Key, Flags, Expire, 0),
                {ok, 0}
      end;
    _ ->
        error_key_does_not_exist
  end;

decr(_, _, _) ->
  {error_amount_must_be_a_positive_integer}.

