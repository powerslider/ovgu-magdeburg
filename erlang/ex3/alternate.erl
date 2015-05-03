%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Infinite stream of repeated applications of a list

-module(alternate).
-export([take/2, ints_from/1, alternate/2]).

take(N, Stream) ->
    take(N, Stream, []).

% use _Stream to avoid head mismatch with take/2
take(0, _Stream, Acc) ->
    lists:reverse(Acc);

take(N, Stream, Acc) ->
    {Value1, Value2, NewStream} = Stream(),
    take(N - 1, NewStream, [Value1, Value2 | Acc]).

ints_from(N) ->  [N | fun() -> ints_from(N + 1) end].

alternate(List1, List2) ->
    Alternate = fun([],[], Alternate) -> 
                    Alternate(List1, List2, Alternate);
                ([H1 | T1], [H2 | T2], Alternate) ->
                    {H1, H2, fun() -> Alternate(T1, T2, Alternate) end}
                end,
    Alternate(List1, List2, Alternate).
