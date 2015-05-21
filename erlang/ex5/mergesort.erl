%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Mergesort algorithm

-module(mergesort).
-export([mergesort/1, sort/2]).

sort(List, Parent) ->
    if
        length(List) == 1 ->
            % send list directly
            Parent ! {List, self()};
        true ->
            {Left, Right} = split(List, [], []),
            LeftSort = spawn(?MODULE, sort, [Left, self()]),
            RightSort = spawn(?MODULE, sort, [Right, self()]),
            receive
                {ResultLeft, LeftSort} ->
                    receive
                        {ResultRight, RightSort} ->
                            % send merged sorted partitions
                            Parent ! {merge(ResultLeft, ResultRight), self()}
                    end
            end
    end.

mergesort([]) ->
    [];
mergesort(List) ->
    Sort = spawn(?MODULE, sort, [List, self()]),
    receive
        {Result, Sort} ->
            Result
    end.


% split list to two parts
split([], Left, Right) ->
    {Left, Right};
split([Head|Tail], Left, Right) ->
    split(Tail, Right ++ [Head], Left).

% merge two lists to one
merge(Left, []) ->
    Left;
merge([], Right) ->
    Right;
merge([HL|TL], [HR|TR]) ->
    if
        HL < HR ->
            [HL|merge(TL, [HR|TR])];
        true ->
            [HR|merge([HL|TL], TR)]
    end.


%7> mergesort:mergesort([3,5,3,7,9,11,10,3,2,7,7]).
%[2,3,3,3,5,7,7,7,9,10,11]

