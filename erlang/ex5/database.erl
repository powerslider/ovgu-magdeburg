%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Database server storing a list of values and clients being 
%%      able to get or set those values

-module(database).
-export([start/0, server/1, get/2, set/3]).

% start one db server process
start() ->
    spawn(?MODULE, server, [dict:new()]).

% receive requests from client for getting or setting new values
% in the dictionary
server(Storage) ->
    receive
        {_, set, Key, Value} ->
            KeyExists = dict:is_key(Key, Storage),
            case KeyExists of
                true -> server(dict:update(Key, fun(_) -> Value end, Storage));
                false -> server(dict:store(Key, Value, Storage))
            end;
            %From ! {self(), KeyExists};
        {From, get, Key} ->
            KeyExists = dict:is_key(Key, Storage),
            case KeyExists of
                true -> From ! {self(), dict:fetch(Key, Storage)};
                false -> From ! {error, self()}
            end,
            server(Storage)
    end.

% send get request with the requested Key
% and receive its Value
get(Pid, Key) ->
    Pid ! {self(), get, Key},
    receive
        {_, Value} -> Value
    end.

% send set request with the requested Key,
% the new Value to be set and receive confirmation
% for success
set(Pid, Key, Value) -> 
    Pid ! {self(), set, Key, Value}.

%% 1> Pid = database:start().
%% <0.35.0>
%% 2> database:set(Pid, 1, "foo").
%% {<0.33.0>,set,1,"foo"}
%% 3> database:get(Pid, 1).
%% "foo"
%% 4> database:set(Pid, 2, "bar").
%% {<0.33.0>,set,2,"bar"}
%% 5> database:get(Pid, 2).
%% "bar"
%% 6> database:set(Pid, 1, "foo-bar").
%% {<0.33.0>,set,1,"foo-bar"}
%% 7> database:get(Pid, 1).
%% "foo-bar"
