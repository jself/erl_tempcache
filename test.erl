-module(test).
-export([start/0, start_get/0, start_set/0]).

random() ->
    list_to_binary(os:cmd("dd if=/dev/urandom bs=32000 count=1")).

start() ->
    cache:start().

start_set() ->
    L = lists:seq(1, 100000),
    R = random(),
    lists:foreach(fun(Li) ->
                io:format("Time: ~p~n",[timer:tc(cache, set, [Li, R])])
        end, L),
    done.

start_get() ->
    L = lists:seq(1, 30000),
    lists:foreach(fun(Li) ->
                io:format("Time: ~p~n", [timer:tc(cache, get, [Li])])
        end, L),
    done.


