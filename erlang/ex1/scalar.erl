%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Function to return the scalar product of 2 lists

-module(scalar).
-export([scalar_product/2]).

% empty lists - product is zero
scalar_product([], []) -> 0;

% if either list is empty return it
scalar_product(List, []) -> List;
scalar_product([], List) -> List;

% multiply heads of both lists and do it recursively
% for their tails
% @throws badarith if the lists are not from the same dimension
scalar_product([H1 | T1], [H2 | T2]) ->
    try H1 * H2 + scalar_product(T1, T2)
    catch
        error:Error -> {error, Error}
    end.


