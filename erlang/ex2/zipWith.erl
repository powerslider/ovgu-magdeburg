%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc ZipWith  function - apply a function to two infinite lists

-module(zipWith).
-export([take/2, fibs/1, zipWith/3, fib/0]).

% this evaluates the lazy list and just returns normal one
eval(L) when is_list(L) -> L; 
eval(LazyL) when is_function(LazyL, 0) -> LazyL().

take(0, _) -> [];

take(N, [H | LazyT]) -> 
    [H | take(N-1, eval(LazyT))].

% forcing evaluation of tail and converting it to a normal list;
% get only the tail and you will always have the next element already calculated
drop(0, T) -> eval(T);
drop(N, [_|LazyT]) -> drop(N - 1, eval(LazyT)).

zipWith(F, [H1|L1], [H2|L2]) -> 
    [F(H1, H2) | fun() -> zipWith(F, eval(L1), eval(L2)) end].

fib() -> [0, 1 | fun() -> zipWith(
                            fun(X, Y) -> X + Y end, fib(), drop(1, fib())) end ].

fibs(N) -> take(N, fib()).

