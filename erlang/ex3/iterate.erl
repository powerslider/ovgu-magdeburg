%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Infinite stream of repeated applications of F to X

-module(iterate).
-export([iterate/2, take/2]).

take(N, Stream) ->
    take(N, Stream, []).

% use _Stream to avoid head mismatch with take/2
take(0, _Stream, Acc) ->
    lists:reverse(Acc);

take(N, Stream, Acc) ->
    {Value, NewStream} = Stream(),
    take(N - 1, NewStream, [Value | Acc]).

iterate(F, X) ->
    Iterate = fun(Acc, Iterate) ->
                      {Acc, fun() -> Iterate(F(Acc), Iterate) end}
              end,
    fun() -> Iterate(X, Iterate) end.

