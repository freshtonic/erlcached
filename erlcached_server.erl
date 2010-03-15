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
cache_delete(Key, TimeSeconds) -> 
  gen_server:call(?MODULE, {cache_delete, Key, TimeSeconds}).
cache_incr(Key, Amount) -> 
  gen_server:call(?MODULE, {cache_incr, Key, Amount}).
cache_decr(Key, Amount) -> 
  gen_server:call(?MODULE, {cache_decr, Key, Amount}).
cache_stats(Key) -> 
  gen_server:call(?MODULE, {cache_stats, Key}).
cache_stats() ->
  gen_server:call(?MODULE, {cache_stats}).

init([]) -> {ok, ets:new(?MODULE, [set, public])}.

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
  {reply, [get_value(Tab, Key) || {Key} <- Keys], Tab};

handle_call({cache_delete, Key, TimeSeconds}, _From, Tab) ->
  case is_key_set(Tab, Key) of
    true -> spawn(fun() -> 
                    receive
                    after TimeSeconds * 1000 ->
                      ets:delete(Tab, Key)
                    end
                  end),
        {reply, ok, Tab};
    false -> {reply, error_key_not_set, Tab}
  end;

handle_call({cache_incr, Key, Amount}, _From, Tab) ->
  case incr(Tab, Key, Amount) of
    {ok, NewAmount} -> {reply, {Key, NewAmount}, Tab};
    error_key_does_not_exist -> {reply, {error, key_does_not_exist, Key}, Tab}
  end;

handle_call({cache_decr, Key, Amount}, _From, Tab) ->
  case decr(Tab, Key, Amount) of
    {ok, NewAmount} -> {reply, {Key, NewAmount}, Tab};
    error_key_does_not_exist -> {reply, {error, key_does_not_exist, Key}, Tab}
  end.

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

