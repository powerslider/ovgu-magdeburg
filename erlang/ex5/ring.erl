%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Ring of N processes that are connected in a way that the 
%%      previous one passes its result to the next one.

-module(ring).
-export([go_ring/3, start/1]).

go_ring(N, M, Msg) ->
    Pid = spawn(?MODULE, start, [N]),
    send(Pid, N, M, Msg).

start(0) -> ok;
start(N) -> 
    Pid = spawn(?MODULE, start, [N - 1]),
    listen(Pid).

listen(Pid) -> 
    receive
        {M, Msg} ->
            io:format("~w: ~s~n", [M, Msg]),
            Pid ! {M - 1, Msg},
            listen(Pid);
        quit ->
            Pid ! quit
    end.

send(Pid, _, 0, _) ->
    timer:sleep(500),
    Pid ! quit;

send(Pid, N, M, Msg) ->
    Pid ! {N, Msg},
    send(Pid, N, M - 1, Msg).

%%ring:go_ring(5,5,"foo").
%%5: foo
%%5: foo
%%4: foo
%%5: foo
%%4: foo
%%3: foo
%%5: foo
%%4: foo
%%3: foo
%%5: foo
%%4: foo
%%2: foo
%%3: foo
%%4: foo
%%2: foo
%%3: foo
%%1: foo
%%2: foo
%%3: foo
%%1: foo
%%2: foo
%%1: foo
%%2: foo
%%1: foo
%%1: foo
%%quit

