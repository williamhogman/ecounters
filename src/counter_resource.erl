-module(counter_resource).
-export([
	 init/1,
	 to_html/2,
	 to_json/2,
	 content_types_provided/2,
	 resource_exists/2,
	 allowed_methods/2,
	 process_post/2
]).

-include_lib("webmachine/include/webmachine.hrl").

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.


content_types_provided(ReqData, State) ->
    Types = [
	     {"text/html", to_html},
	     {"application/json", to_json}
    ],
    {Types, ReqData, State}.


allowed_methods(ReqData, State) ->
    {['GET', 'HEAD', 'POST'], ReqData, State}.
    

resource_exists(ReqData, State) ->
    Result = case wrq:path_info(counter_id, ReqData) of
	undefined -> {ok, false};
	X -> counters:exists(X)
    end,
    Return = case Result of
		 {ok, Res} -> Res;
		 {error, _} = Error -> Error
	     end,
    {Return, ReqData, State}.

read_body(ReqData) ->
    case wrq:req_body(ReqData) of
	undefined -> {error, no_body};
	<<>> -> {error, no_body};
	Body -> mochijson:binary_decode(Body)
    end.

parse_counter_operation({struct, Data}) ->
    case proplists:get_value(<<"method">>, Data) of
	undefined -> {error, invalid_body};
	<<"increment">> -> {ok, increment};
	<<"decrement">> -> {ok, decrement};
	_ -> {error, unknown_operation}
    end.

alter_counter(Which, increment) -> counters:increment(Which);
alter_counter(Which, decrement) -> counters:decrement(Which).

    
process_post(ReqData, State) ->  
    Obj = read_body(ReqData),
    Id = wrq:path_info(counter_id, ReqData),
    Res = case parse_counter_operation(Obj) of
	      {ok, Op} -> alter_counter(Id, Op);
	      {error, _} = Err -> Err
	  end,
    case Res of
	{ok, _} -> {true, ReqData, State};
	{error, _} = Err2 -> {Err2, ReqData, State}
    end.
		   
    



-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.
to_html(ReqData, State) ->
    Id = wrq:path_info(counter_id, ReqData),
    Result = case counters:get_counter(Id) of
		 {ok, X} -> integer_to_list(X);
		 {error, _} = Err -> Err
	     end,
    {Result, ReqData, State}.

json_body(QS) -> mochijson:encode({struct, QS}).

to_json(ReqData, State) ->
    Id = wrq:path_info(counter_id, ReqData),
    Res = case counters:get_counter(Id) of
	      {ok, Val} -> json_body([{value, Val}]);
	      {error, _} = ErrCounter -> ErrCounter
	  end,
    {Res, ReqData, State}.
