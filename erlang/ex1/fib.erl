%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Fibonacci functions

-module(fib).
-export([fib/1, fib_tail/1]).

% standard recursion (very slow!)
fib(1) -> 0;

fib(2) -> 1;

fib(N) when N >= 0 ->
    fib(N - 1) + fib(N - 2).

% tail recursive version 
fib_tail(N) -> 
    fib_tail(N, 0, 0, 0).

% keep the end of the list in End (recursion stops when N = End),
% increment N, starting from 0, 
% sum SecondLast and Last
% and pass Last as SecondLast after each call
fib_tail(End, N, Last, SecondLast) ->
    case N of
        End -> Last + SecondLast;
        0 -> fib_tail(End, 1, 0, 0);
        1 -> fib_tail(End, 2, 1, 0);
        _ -> fib_tail(End, N + 1, SecondLast + Last, Last)
    end.


