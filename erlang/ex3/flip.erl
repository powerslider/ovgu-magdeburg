%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Currying function for partial application when swapping args

-module(flip).
-export([flip/1, flipc/1]).

flip(Foo2) ->
    fun(X, Y) -> Foo2(Y, X) end.

flipc(Foo2) ->
    fun(X) -> 
            (fun(Y) ->
                    Foo2(Y, X)
            end)
    end.

% (flip:flip(fun(X, Y) -> X - Y end))(5, 6).
% 1

% ((flip:flipc(fun(X, Y) -> X - Y end))(5))(-6).
% 1
