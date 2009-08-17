-module(cache_free).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, start_link/2]).
-export([get_mem_size/1, check_table/0]).
-include_lib("stdlib/include/ms_transform.hrl").
-behaviour(gen_server).

get_max_size() ->
    16000000.

check_table() ->
    gen_server:call(?MODULE, check),
    {ok, checking}.

start_link(Tab, Internal) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Tab, Internal], []).

init([Tab, Internal]) ->
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


%    F = ets:fun2ms(fun({Key, {Timeout, _, _}}) when Timeout /= 0, Timeout > Now -> Key end),
%    L = ets:select(Internal, F),
    lists:foreach(fun(Item) -> ets:delete(Tab, Item) end, L),
    L.
    
delete_first_item(Tab, Internal, []) ->
    %% we want to keep the most accessed items in the cache, so we add
    %% a minute to the expiration per time it's been accessed
    %    F = ets:fun2ms(fun({Key, {_, Accessed, Created}}) -> {Key, (Accessed + 1)*60 + Created} end),
    %L = ets:select(Internal, F),
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
terminate(_Reason, _State) -> ok.
code_change(_Oldversion, State, _Extra) -> {ok, State}.



