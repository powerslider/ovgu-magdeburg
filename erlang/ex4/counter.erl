-module(counter).
-export([start/1, stop/1, increment/1, decrement/1, show/1, go/1]).

start(N) ->
    spawn(?MODULE, go, [N]).

stop(P) ->
    P ! {stop, self()}.

increment(P) ->
    P ! {increment, self()}.

decrement(P) ->
    P ! {decrement, self()}.

show(P) ->
    P ! {show, self()},
    receive
        {RetVal, _} -> RetVal
    end.

go(N) ->
    receive
        {increment, _} ->
            go(N + 1);
        {decrement, _} ->
            go(N - 1);
        {show, S} ->
            S ! {N, self()},
            go(N);
        {stop, _} ->
            0
    end.
