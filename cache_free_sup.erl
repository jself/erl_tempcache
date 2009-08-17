-module(cache_free_sup).
-behaviour(supervisor).
-export([start_link/2, init/1]).

start_link(Tab, Internal) ->
    supervisor:start_link(cache_free_sup, [Tab, Internal]).

init(Args) ->
    io:format("Starting free_sup~n",[]),
    {ok, {{one_for_one, 2, 30},
            [{cache, {cache_free, start_link, Args},
                    permanent, brutal_kill, worker, [cache_free]}]}}.
