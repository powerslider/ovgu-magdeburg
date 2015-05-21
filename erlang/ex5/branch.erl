%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc 

-module(branch).
-export([go/1, loop/2]).

go(Lim) -> 
    Root = spawn(branch, loop, [1, Lim]),
    Root ! {createdBy, self()},
	io:format("process ~w created at toplevel.~n",
              [Root]).

loop(Lim, Lim) -> 
    receive
        {createdBy, Parentpid} -> 
            io:format("process ~w created by ~w at count ~w.~n",
                [self(), Parentpid, Lim]),
            Parentpid ! {confirmed, self()}
    end;

loop(Count, Lim) when Count =< Lim -> 
    Count1 = Count + 1,
    Child1 = spawn(branch, loop, [Count1, Lim]), 
    Child2 = spawn(branch, loop, [Count1, Lim]),
    Child1 ! {createdBy, self()},
    Child2 ! {createdBy, self()}, 
    receive
        {createdBy, Parentpid} ->
           io:format("process ~w created by ~w at count ~w.~n",
                [self(),Parentpid,Count]),		   
            Parentpid!{confirmed, self()}
    end,
    receive 
	    {confirmed, Childpid1} ->
	        io:format("process ~w confirmed creation to ~w at count ~w.~n",
		        [Childpid1,self(),Count])
    end,
    receive 
	    {confirmed, Childpid2} ->
	        io:format("process ~w confirmed creation to ~w at count ~w.~n",
		        [Childpid2,self(),Count])
    end.


%% 10> branch:go(3).
%% process <0.178.0> created at toplevel.
%% process <0.178.0> created by <0.33.0> at count 1.
%% process <0.179.0> created by <0.178.0> at count 2.
%% process <0.180.0> created by <0.178.0> at count 2.
%% process <0.181.0> created by <0.179.0> at count 3.
%% process <0.182.0> created by <0.179.0> at count 3.
%% process <0.183.0> created by <0.180.0> at count 3.
%% process <0.184.0> created by <0.180.0> at count 3.
%% process <0.181.0> confirmed creation to <0.179.0> at count 2.
%% process <0.183.0> confirmed creation to <0.180.0> at count 2.
%% process <0.179.0> confirmed creation to <0.178.0> at count 1.
%% process <0.182.0> confirmed creation to <0.179.0> at count 2.
%% process <0.184.0> confirmed creation to <0.180.0> at count 2.
%% process <0.180.0> confirmed creation to <0.178.0> at count 1.
%% ok
