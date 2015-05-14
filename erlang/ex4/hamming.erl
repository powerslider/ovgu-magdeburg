%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Alternating order of two lazy lists as pairs of entries from
%%      both lists.

-module(hamming).
-export([ints/0, ints_from/1, take/2, hamming/0]).

ints() -> ints_from(1).

ints_from(N) -> [N | fun() -> ints_from(N + 1) end].

take(0, _) -> [];
take(N, [H | LazyT]) -> [H | take(N - 1, LazyT())].

map(F, [H | T]) ->
    [F(H) | fun() -> map(F, T()) end].

merge([], List2) -> List2;
merge(List1, []) -> List1;
merge([H1|T1], [H2|_] = List2) when H1 =< H2 ->
    [H1| fun()-> merge(T1(), List2) end];
merge(List1, [H2 | T2]) ->
    [H2 | fun()-> merge(List1,T2) end].


scaleStream(N, Lazy) ->
    fun() -> map(fun(X) -> X * N end, Lazy()) end.

mergeStream([H1 | T1], [H2 | T2]) ->
    [merge(H1, H2) | mergeStream(T1, T2)].

hamming() -> lists:usort(
    [mergeStream(
      mergeStream(
        scaleStream(2, hamming()),
        scaleStream(3, hamming())),
      scaleStream(5, hamming())) | ints()]).



