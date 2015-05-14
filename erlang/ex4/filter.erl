%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Alternating order of two lazy lists as pairs of entries from
%%      both lists.

-module(filter).
-export([ints/0, take/2, filter/2, takeOdds/1]).

ints() -> ints_from(0).

ints_from(N) -> [N | fun() -> ints_from(N + 1) end].

take(0, _) -> [];
take(N, [H | LazyT]) -> [H | take(N - 1, LazyT())].

filter(_, []) -> [];
filter(Pred, [H | T]) ->
        case Pred(H) of
            true -> [H |fun() -> filter(Pred, T()) end];
            false -> filter(Pred, T())
        end.

takeOdds(N) ->
    take(N,
      filter(fun(X) -> X rem 2 /= 0 end, ints())
    ).



