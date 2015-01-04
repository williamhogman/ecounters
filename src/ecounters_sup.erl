-module(ecounters_sup).
-behaviour(supervisor).

%% External exports
-export([
  start_link/0
]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [ecounters_config:web_config()]},
           permanent, 5000, worker, [mochiweb_socket_server]},
    PoolArgs = [
		{size, 10},
		{max_overflow, 20},
		{name, {local, eredis_pool}},
		{worker_module, eredis}
	       ],
    Poolboy = poolboy:child_spec(eredis_pool, PoolArgs),
    Processes = [Web, Poolboy],
    {ok, { {one_for_one, 10, 10}, Processes} }.
