% Module for accessing counters
-module(counters).
-export([
	 get_counter/1,
	 exists/1,
	 increment/1,
	 increment/2,
	 decrement/1,
	 decrement/2
]).


redis_txn(Command) ->
    poolboy:transaction(eredis_pool, fun(Worker) ->
					     eredis:q(Worker, Command)
				     end).

strict_string_to_int({error, _} = Data) -> Data;
strict_string_to_int({Result, []}) -> {ok, Result};
strict_string_to_int({_Result, _Something}) -> {error, no_integer}.

redis_to_int(I) -> 
    strict_string_to_int(string:to_integer(I)).

redis_to_bool(B) ->
    case redis_to_int(B) of
	{ok, 1} -> {ok, true};
	{ok, 0} -> {ok, false};
	{ok, _} -> {error, no_bool};
	{error, _} = Error -> Error
    end.

handle_redis_int_response({ok, undefined}) -> {ok, 0};
handle_redis_int_response({ok, N}) when is_list(N) -> redis_to_int(N);
handle_redis_int_response({ok, N}) when is_binary(N) -> handle_redis_int_response({ok, binary_to_list(N)});
handle_redis_int_response({error, _} = Err) -> Err.

increment(Which) -> increment(Which, 1).
increment(Which, N) ->
    NStr = integer_to_list(N),
    Res = redis_txn(["HINCRBY", "counters", Which, NStr]),
    handle_redis_int_response(Res).

decrement(Which) -> decrement(Which, -1).
decrement(Which, N) -> increment(Which, -N).
			  
get_counter(Which) when is_list(Which) ->
    Res = redis_txn(["HGET", "counters", Which]),
    handle_redis_int_response(Res).


exists(Which) when is_list(Which) ->
    case redis_txn(["HEXISTS", "counters", Which]) of
	{ok, Res} -> redis_to_bool(binary_to_list(Res));
	{error, _} = Error -> Error
    end.
