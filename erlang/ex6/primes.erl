%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Prime numbers count in a specified range. The workload
%%      is split between 2 parallel processes which process the
%%      halves of the number range.

-module(primes).
-export([go/3, init/4]).

go(From, To, Min) ->
    init(From, To, Min, self()),
    receive
        Result ->
            Result
    end.

init(From, To, Min, Pid) ->
    if
        % when the interval reaches the minimum threshold
        % report the count of prime numbers in the range
        To - From =< Min ->
            Pid ! primeCount(From, To);
    true ->
        % find middle of the of the range
        Middle = From + (length(lists:seq(From, To)) div 2),
        
        % start recursive processes for each half of the range
        spawn(?MODULE, init, [From, Middle, Min, self()]),
        spawn(?MODULE, init, [Middle, To, Min, self()]),
        
        % sum temporal results from both parallel processes
        receive
            Left ->
                receive
                    Right ->
                        Left + Right
                end
        end
    end.

sieve([]) -> [];
sieve([Head|Tail]) ->
    [Head | sieve([X || X <- Tail, X rem Head /= 0])].

intersection(List1, List2) ->
    [Y || X <- List1, Y <- List2, X==Y].

primeCount(From, To) ->
    NumRange = lists:seq(From,To),
    Primes = sieve(lists:seq(2,To)),
    length(intersection(NumRange, Primes)).

%% The smaller the minimum threshold for the interval the slower it is to compute because
%% the sieve is computed for every element on every partitioning.
%%
%% 1> timer:tc(primes, go, [10000, 30000, 25000]).
%% {874197,2016}
%% 2> timer:tc(primes, go, [10000, 30000, 20000]).
%% {876231,2016}
%% 3> timer:tc(primes, go, [10000, 30000, 10000]).
%% way too slooooooooow
