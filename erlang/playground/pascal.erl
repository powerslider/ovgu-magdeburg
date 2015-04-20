-module(pascal).
-export([add_row/1, triangle/1]).

% setup initial list and empty result list
add_row(Initial) -> 
    add_row(Initial, 0, []).

% when current row is fully read return the final row 
add_row([], 0, Final) -> 
    [0 | Final];

% list is split into head and tail, Last has initial value of 0 
% and gets updated with the current head of the input list,
% New is the tail of the newly calculated list
add_row([Head | Tail], Last, New) ->
    add_row(Tail, Head, [Last + Head | New]).

% setup count of rows for the triangle
triangle(Rows) -> 
    % initialize a list of rows, with the initial row being [0, 1, 0]
    triangle([[0, 1, 0]], 0, Rows).

triangle(List, Count, Rows) when Count >= Rows ->
    lists:reverse(List);

% calculate all triangle rows by using the add_row function
triangle(List, Count, Rows) ->
    % get previous row by getting only the head of the list of rows 
    % and ignoring the tail (the other rows)
    [Previous |_] = List, 
    triangle([add_row(Previous) | List], Count + 1, Rows).

