-module(test).
-export([start/0, start_get/0, start_set/0]).

random() ->
%    list_to_binary(os:cmd("dd if=/dev/urandom bs=32000 count=1")).
    lists:seq(0,500).

start() ->
    cache:start().


start_set() ->
    L = lists:seq(1, 10000),
    R = random(),
    {Time, ok} = timer:tc(plists, foreach, [fun (Li) ->
                    cache:set(Li, R) end, L, {processes, 10}]),
    io:format("Total time: ~p microseconds~n",[Time]),
    io:format("Time per call: ~p microseconds~n",[Time/length(L)]).

start_get() ->
    L = lists:seq(1, 10000),
    {Time, ok} = timer:tc(plists, foreach, [fun (Li) ->
                    cache:get(Li) end, L, {processes, 10}]),
    io:format("Total time: ~p microseconds~n",[Time]),
    io:format("Time per call: ~p microseconds~n",[Time/length(L)]).

