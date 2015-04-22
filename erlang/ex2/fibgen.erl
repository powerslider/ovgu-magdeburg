%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Functions to get an element from an endless 
%%      Fibonacci list.

-module(fibgen).
-export([nthFib/1]).

% get the nth element of an endless list (list is 0-based)
nth(N, Lazy) ->
    if
        % if it is the first element return the head of the list
        N == 0 -> hd(Lazy());

        % else get the element from the tail of the list
        true -> nth(N - 1, tl(Lazy()))
    end.

% lazy function for generation of Fibonacci endless list
fibgen(A, B) ->
    fun() -> [A | fibgen(B, A + B)] end.

% get Nth element from the Fibonacci endless list
nthFib(N) -> nth(N, fibgen(0, 1)).



