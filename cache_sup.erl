-module(cache_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    Tab = ets:new(cache_tab, [set, public]),
    Internal = ets:new(cache_internal, [set, public]),
    supervisor:start_link(cache_sup, [Tab, Internal]).

init(Args) ->
    io:format("Starting~n",[]),
    io:format("Args: ~p~n",[Args]),
    {ok, {{one_for_one, 2, 30},
            [
                {cache_free_sup, {cache_free_sup, start_link, Args},
                    permanent, infinity, supervisor, [cache_free_sup]},
                {cache_server_sup, {cache_server_sup, start_link, Args},
                    permanent, infinity, supervisor, [cache_server_sup]}
            ]}}.
