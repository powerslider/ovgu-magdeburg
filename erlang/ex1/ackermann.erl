%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Ackermann function

-module(ackermann).
-export([ackermann/2]).


ackermann(0, Y) -> Y + 1;

ackermann(X, 0) -> ackermann(X - 1, 1);

ackermann(X, Y) -> ackermann(X - 1, ackermann(X, Y - 1)).

%% NOTE: If the parameters are positive integers they will be eventually 
%% reduced and the function will terminate, otherwise it is undecidable in reasonable time.
%% Regarding time complexity, it belongs to a class of functions called superexponential functions
%% which could find an answer after 2^2^2... operations.
%%
%% Examples:
%% 2> ackermann:ackermann(4,1).  ->  40s
%% 65533
%% 3> ackermann:ackermann(3, 11).   ->  6s
%% 16381

