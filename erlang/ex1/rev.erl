%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Function for reversing a list

-module(rev).
-export([reverse/1, reverse_standard/1]).

% standard library is faster
reverse_standard(List) -> 
    lists:reverse(List).

% initialize empty result list
reverse(List) -> reverse(List, []).

% when list is already reversed
reverse([], ReversedList) -> ReversedList;

% reverse by adding the head of the input list to the new list
reverse([Head | Tail], ReversedList) ->
    reverse(Tail, [Head | ReversedList]).
