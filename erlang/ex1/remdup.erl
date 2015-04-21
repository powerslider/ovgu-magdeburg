%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Function to remove duplicates in a list

-module(remdup).
-export([remdup/1]).

remdup(List) -> remdup(List, []).

% when input list is read, reverse the result in order
% to keep the order of the original list
remdup([], Result) -> lists:reverse(Result);

% add head of input list to result list and remove from
% it the tail of the input list in order to disambiguate it
remdup([Head | Tail], Result) ->
    remdup(Tail, [Head | Result] -- Tail).
