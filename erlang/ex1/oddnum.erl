%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Function to remove odd numbers in a list

-module(oddnum).
-export([remove_odd/1, remove_odd_comp/1]).

% remove odd using list comprehension 
remove_odd_comp(List) -> 
    [X ||  X <- List, X rem 2 == 0].

remove_odd(List) -> 
    remove_odd(List, []).

% when input is read, reverse list to keep original order
remove_odd([], Result) -> lists:reverse(Result);

% add only even nums to Result list
remove_odd([Head | Tail], Result) ->
    %% odd or not
    Odd = Head rem 2,
    case Odd of
        % if even add head of input list to Result
        0 -> remove_odd(Tail, [Head | Result]);

        % if odd simply do not add it to Result
        1 -> remove_odd(Tail, Result)
    end.



