-module(cache_sup).
-behaviour(supervisor).
-export([start_link/0, start_link/1, init/1]).

start_link() ->
    Tab = ets:new(cache_tab, [set, public]),
    Internal = ets:new(cache_internal, [set, public]),
    supervisor:start_link(cache_sup, [Tab, Internal]).

start_link(shell) ->
    Tab = ets:new(cache_tab, [set, public]),
    Internal = ets:new(cache_internal, [set, public]),
    {ok, Pid} = supervisor:start_link(cache_sup, [Tab, Internal]),
    unlink(Pid).


init(Args) ->
    {ok, {{one_for_one, 2, 30},
            [
                {cache, {cache, start_link, Args},
                        permanent, brutal_kill, worker, [cache]},
            {cache_free, {cache_free, start_link, Args},
                    permanent, brutal_kill, worker, [cache_free]}
            ]
        }}.
