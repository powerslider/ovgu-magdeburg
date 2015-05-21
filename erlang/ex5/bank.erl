%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Bank account shared between two users. When the balance falls below 0
%%      signal that there is no money available.

-module(bank).
-export([go_bank/1, account/1, client/1]).

% spawn one account process and 2 client processes
go_bank(N) ->
    Pid = spawn(?MODULE, account, [N]),
    spawn(?MODULE, client, [Pid]),
    spawn(?MODULE, client, [Pid]).

% check availability of the account after receiving 
% client requests
account(Balance) ->
    receive
        {From, Request} ->
            if
                Balance >= 0 ->
                    From ! {self(), true},
                    io:format("Request for ~w received~n", [Request]),
                    account(Balance - Request);
                Balance < 0 ->
                    From ! {self(), false},
                    io:format("Request for ~w denied, balance insufficient~n", [Request]),
                    account(Request)
            end
    end.

% send account requests in a random manner regarding their 
% size and receive response for account status
client(Pid) ->
    Pid ! {self(), random:uniform(50)},
    receive
        {_, true} ->
            timer:sleep(random:uniform(100)),
            client(Pid);
        {_, false} -> 
            io:format("Account ~w is empty!~n", [Pid])
    end.

%% 8> bank:go_bank(500).
%% Request for 23 received
%% Request for 23 received
%% <0.69.0>
%% Request for 48 received
%% Request for 48 received
%% Request for 16 received
%% Request for 16 received
%% Request for 46 received
%% Request for 46 received
%% Request for 24 received
%% Request for 24 received
%% Request for 8 received
%% Request for 8 received
%% Request for 35 received
%% Request for 35 received
%% Request for 28 received
%% Request for 28 received
%% Request for 23 received
%% Request for 23 received
%% Request for 1 denied, balance insufficient
%% Account <0.67.0> is empty!
%% Request for 1 received
%% Request for 24 received
%% Request for 16 denied, balance insufficient
%% Account <0.67.0> is empty!



