%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Demo of recursive functions for countdown and countup.

-module(count).
-export([countdown/1, countup/1]).

countdown(From) when From > 0 -> 
    io:format("~w!~n", [From]),
    countdown(From - 1);

countdown(From) ->
    io:format("blastoff!~n").

countup(Limit) ->
    countup(1, Limit).

countup(Count, Limit) when Count =< Limit ->
    io:format("~w!~n", [Count]),
    countup(Count + 1, Limit);

countup(Count, Limit) -> 
    io:format("Finished!~n").
