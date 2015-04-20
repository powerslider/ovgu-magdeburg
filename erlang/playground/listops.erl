-module(listops).
-export([product/1]).

product([]) -> 0; % List is empty, return zero

product(List) -> 
    product(List, 1). 

product([], Product) -> Product; % When list empty, stop and report

product([Head | Tail], Product) -> 
    product(Tail, Product * Head).

