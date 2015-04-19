%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Demo of recursive and tail recursive factorial.

-module(fact).
-export([factorial/1, factorialTR/1]).

factorial(N) when N > 1 ->
    io:format("Calling from ~w.~n", [N]),
    Result = N * factorial(N - 1),
    io:format("~w! yields ~w~n", [N, Result]),
    Result;

factorial(N) when N =< 1 -> 
    io:format("Calling from 1.~n"),
    io:format("1! yields 1~n"),
    1.

factorialTR(N) ->
    factorialTR(1, N, 1).

factorialTR(Current, N, Result) when Current =< N ->
    NewResult = Current * Result,
    io:format("~w! yields ~w~n", [Current, NewResult]),
    factorialTR(Current + 1, N, NewResult);

factorialTR(Current, N, Result) ->
    io:format("Finished!~n"),
    Result.
