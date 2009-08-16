-module(cache_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(cache_sup, []).

init(_) ->
    {ok, {{one_for_one, 2, 30},
            [{cache, {cache, start_link, []},
                    permanent, brutal_kill, worker, [cache]}]}}.
