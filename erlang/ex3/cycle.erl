%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Infinite stream of repeated applications of a list

-module(cycle).
-export([take/2, ints_from/1, cycle/1]).

take(N, Stream) ->
    take(N, Stream, []).

% use _Stream to avoid head mismatch with take/2
take(0, _Stream, Acc) ->
    lists:reverse(Acc);

take(N, Stream, Acc) ->
    {Value, NewStream} = Stream(),
    take(N - 1, NewStream, [Value | Acc]).

ints_from(N) ->  [N | fun() -> ints_from(N + 1) end].

cycle(List) ->
    Cycle = fun([], Cycle) -> 
                Cycle(List, Cycle);
            ([H | T], Cycle) ->
                {H, fun() -> Cycle(T, Cycle) end}
            end,
    fun() -> Cycle(List, Cycle) end.
