%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Alternating order of two lazy lists as pairs of entries from
%%      both lists.

-module(sieve).
-export([ints_from/1, take/2, sieve/1, takePrimes/1]).


ints() -> ints_from(2).

ints_from(N) -> [N | fun() -> ints_from(N + 1) end].

take(0, _) -> [];
take(N, [H | LazyT]) -> [H | take(N - 1, LazyT())].

filter(_, []) -> [];
filter(Pred, [H | T]) ->
        case Pred(H) of
            true -> [H |fun() -> filter(Pred, T()) end];
            false -> filter(Pred, T())
        end.

sift(Current, List) ->
    filter(fun(X) -> X rem Current /= 0 end, List).


sieve([H | T]) ->
    [H | fun() -> sieve(sift(H, T())) end].

takePrimes(N) ->
    take(N,
      sieve(ints())
    ).



