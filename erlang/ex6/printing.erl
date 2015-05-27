%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Printing function that uses processes but hidden from the user.
%%      Uses a registered process to send a string to stdout.

-module(printing).
-export([printer/0, startPrinter/0, stopPrinter/0, print/1]).

startPrinter() ->
    receive
        String ->
            io:format("~s~n", [String])
    end,
    startPrinter().

printer() ->
    register(?MODULE, Pid = spawn(?MODULE, startPrinter, [])),
    Pid.

print(String) ->
    ?MODULE ! String.

stopPrinter() ->
    exit(whereis(?MODULE), kill).

%% 6> printing:printer().
%% <0.54.0>
%% 7> printing:print("Erlang rocks!!!").
%% Erlang rocks!!!
%% "Erlang rocks!!!"
%% 8> printing:stopPrinter().
%% true
