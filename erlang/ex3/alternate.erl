%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Alternating order of two lazy lists as pairs of entries from
%%      both lists.

-module(alternate).
-export([take/2, ints_from/1, alternate/2]).


take(N, Stream) ->
    take(N, Stream, []).

% use _Stream to avoid head mismatch with take/2
take(0, _Stream, Acc) -> 
    lists:reverse(Acc);

take(N, Stream, Acc) ->
    {Value1, Value2, NewStream} = Stream(),
    take(N - 2, NewStream, [Value1, Value2 | Acc]).

ints_from(N) ->  [N | fun() -> ints_from(N + 1) end].

alternate(List1, List2) ->
    Alternate = fun([],[], Alternate) -> 
                    Alternate(List1, List2, Alternate);
                ([H1 | T1], [H2 | T2], Alternate) ->
                    {H2, H1, fun() -> Alternate(T1(), T2(), Alternate) end}
                end,
    fun() -> Alternate(List1, List2, Alternate) end.

%alternate(LazyLists) -> 
%    Alternate = fun(L1, L2) ->
%                    alternate(L1, L2)
%                end,
%    apply(
%        fun() ->
%            Alternate(hd(LazyLists), tl(LazyLists))
%        end,
%        LazyLists).


