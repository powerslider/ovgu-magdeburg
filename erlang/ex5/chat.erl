%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Simple Client-Server chat program

-module(chat).
-export([server/0, echo_client/1, test/0]).

server() ->
    receive
        {SenderPid, ReceiverPid, Msg} ->
            ReceiverPid ! {SenderPid, Msg},
            server();
        stop ->
            exit(ok);
        _ ->
            server()
    end.

echo_client(ServerPid) ->
    receive
        {SenderPid, Msg} ->
            io:format("incoming: ~s~n", [Msg]),
            ServerPid ! {self(), SenderPid, Msg},
            echo_client(ServerPid);
        stop ->
            exit(ok)
    end.

test() ->
    % initialize server process 
    Server = spawn(?MODULE, server, []),
    
    %initialize client process by passing server pid
    Client = spawn(?MODULE, echo_client, [Server]),
    
    % server sends welcome message
    Server ! {self(), Client, "Hallo"},
    receive
        % client responds server welcome message
        {_, Msg} ->
            io:format("answer: ~s~n", [Msg])
    end.

%% 2> chat:test().
%% incoming: Hallo
%% answer: Hallo
%% ok

