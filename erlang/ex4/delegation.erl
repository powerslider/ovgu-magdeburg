-module(delegation).
-export([start/3, delegate/4]).


start(F, Args, Ref) ->
    Pid = self(),
    spawn(?MODULE, delegate, [Pid, F, Args, Ref]),
    receive
        {RetVal, Ref, _} -> {RetVal, Ref}
    end.

delegate(Pid, F, Args, Ref) ->
    RetVal = apply(F, Args),
    Pid ! {RetVal, Ref, self()}.
