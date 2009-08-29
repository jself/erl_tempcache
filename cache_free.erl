-module(cache_free).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).
-export([get_mem_size/1, check_table/0, get_max_size/0, start_config/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-behaviour(gen_server).

get_config_val(_, []) ->
    {error, not_found};
get_config_val(Name, [{Name, Value}|_]) ->
    {ok, Value};
get_config_val(Name, [_|T]) ->
    get_config_val(Name, T).

loop_config(Vars) ->
    receive 
        reload ->
            Res = file:consult("settings.cfg"),
            loop_config(Res);
        {From, get, Name} ->
            From!get_config_val(Name, Vars),
            loop_config(Vars);
        change ->
            ?MODULE:loop_confiG(Vars);
        stop ->
            ok;
        _ ->
            loop_config(Vars)
     end.

get_config(Name) ->
    cache_free_config!{self(), get, Name},
    receive
        A -> A
    after 1000 ->
        {error, timeout}
    end.

        
start_config() ->
    {ok, Res} = file:consult("settings.cfg"),
    register(cache_free_config, spawn_link(fun() -> loop_config(Res) end)).
    
get_max_size() ->
    {ok, Size} = get_config(maxsize),
    Size.

check_table() ->
    gen_server:call(?MODULE, check),
    {ok, checking}.

start_link(Tab, Internal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab, Internal], []).

init([Tab, Internal]) ->
    start_config(),
    {ok, {Tab, Internal}}.


get_mem_size(Tab) ->
    L = ets:info(Tab),
    F = fun (X) ->
            case X of 
                {memory, _} -> true;
                _ -> false
                end
        end,
    [{_, H}|_] = lists:filter(F, L),
    H*erlang:system_info(wordsize).

expire(Tab, Internal) ->
    Now = cache:timeout_to_secs(0),
    L = ets:foldl(
        fun({_, {Timeout, _, _}}=A, Acc) ->
                if Timeout /= 0->
                    Acc;
                Timeout > Now ->
                    [A | Acc];
                true ->
                    Acc
                end
        end, [], Internal),


    lists:foreach(fun(Item) -> ets:delete(Tab, Item) end, L),
    L.
    
delete_first_item(Tab, Internal, []) ->
    %% we want to keep the most accessed items in the cache, so we add
    %% a minute to the expiration per time it's been accessed
    L = ets:foldl(
        fun({Key, {_, Accessed, Created}}, Acc) ->
                [{Key, Accessed + 1* 60 + Created}|Acc] end,
            [], Internal),
    
    F2 = fun(A, B) ->
            {_, Value1} = A,
            {_, Value2} = B,
            if 
                Value1 < Value2 -> true;
                true -> false
            end
        end,
    delete_first_item(Tab, Internal, lists:sort(F2, L));
    
delete_first_item(Tab, Internal, [{Key, _}|T]) ->
    catch(ets:delete(Tab, Key)),
    catch(ets:delete(Internal, Key)),
    check_table(Tab, Internal, true, T).


        


check_table(Tab, Internal, Expired, OrderOfDelete) ->
    TabSize = get_mem_size(Tab),
    MaxSize = get_max_size(),
    if
        TabSize > MaxSize ->
            case Expired of
                false->
                    expire(Tab, Internal),
                    check_table(Tab, Internal, true, OrderOfDelete);
                true ->
                    delete_first_item(Tab, Internal, OrderOfDelete)
                end;
        true ->
            ok
    end.

handle_cast(check, {Tab, Internal}) -> 
    check_table(Tab, Internal, false, []),
    {noreply, {Tab, Internal}};
handle_cast(_, State) -> {noreply, State}.

handle_call(check, _From, {Tab, Internal}) -> 
    check_table(Tab, Internal, false, []),
    {reply, checking, {Tab, Internal}};

handle_call(_, _, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.
terminate(_Reason, _State) -> 
    cache_free_config!stop,
    ok.
code_change(_Oldversion, State, _Extra) -> 
    cache_free_config!change,
    {ok, State}.



