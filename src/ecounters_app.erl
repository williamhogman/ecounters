-module(ecounters_app).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

start(_Type, _StartArgs) ->
    ecounters_sup:start_link().

stop(_State) ->
    ok.
