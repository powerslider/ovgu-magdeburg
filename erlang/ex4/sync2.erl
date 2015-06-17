-module(sync2).
-export([delegate/3, complexMaster/3]).


complexMaster(FuncList, ArgsList, CombFunc) ->
    Pid = self(),

    Input = lists:zip(FuncList, ArgsList),

    Pids = lists:map(
             fun(I) -> 
                  spawn(?MODULE, delegate, [Pid, I, make_ref()])
             end, Input),

    Results = lists:map(
        fun(P) -> 
            receive
                {RetVal, _, P} -> RetVal
            end
        end, Pids),

    apply(CombFunc, Results).

delegate(Pid, {F, Args}, Ref) ->
    RetVal = apply(F, Args),
    Pid ! {RetVal, Ref, self()}.

%%  sync:complexMaster([(fun(X, Y) -> X + Y end), (fun(X, Y) -> X + Y end)], [[2, 3], [2, 3]], fun(X, Y) -> X * Y end).
%%  25
