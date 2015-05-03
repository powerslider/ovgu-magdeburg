%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Currying functions replacing elements lists

-module(curry).
-export([curry3/1, replace/3, replaceLL/3]).

% currying function passing its argument to the inner partial functions
curry3(Foo) ->
    fun(X) ->
            (fun(Y) ->
                    (fun(Z) ->
                            Foo(X, Y, Z)
                    end)
            end)
    end.

% replace all occurrences of an element with another element in a list
replace(Elem, NewElem, List) ->
    [(fun(CurrentE, E, NewE) ->
            if
                CurrentE == E -> NewE;
                true -> CurrentE
            end
      end)(X, Elem, NewElem) || X <- List].

% replace all occurrences of an element with another element in a list of lists
replaceLL(Elem, NewElem, ListList) ->
    lists:map(
      (
       (curry3(fun replace/3))
       (Elem)
      )
      (NewElem), ListList).
