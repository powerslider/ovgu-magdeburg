-module(sync).
-export([delegate/4, simpleMaster/4]).


simpleMaster(F1, Args1, F2, Args2) ->
    Pid = self(),
    spawn(?MODULE, delegate, [Pid, F1, Args1, 0]),
    spawn(?MODULE, delegate, [Pid, F2, Args2, 0]),
    receive
        {RetVal1, _, _} -> 
            receive
                {RetVal2, _, _} ->
                    RetVal1 * RetVal2
            end
    end.

delegate(Pid, F, Args, Ref) ->
    RetVal = apply(F, Args),
    Pid ! {RetVal, Ref, self()}.
