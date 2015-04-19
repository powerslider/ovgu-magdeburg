%% @author Tsvetan Dimitrov <tsvetan.dimitrov23@gmail.com>
%% @doc Functions calculating velocities achieved by objects
%% dropped in a vacuum.

-module(drop).
-export([fall_velocity/1]).

%% accepts a tuple as a parameter and extracts its contents
fall_velocity(Where) -> 
    {Planemo, Distance} = Where,
    fall_velocity(Planemo, Distance).


fall_velocity(Planemo, Distance) when Distance >= 0 ->
    Gravity = case Planemo of
        earth -> 9.8;
        moon -> 1.6;
        mars -> 3.71
    end,

    %% calculate velocity from gathered gravity and distance
    Velocity = math:sqrt(2 * Gravity * Distance),

    %% assign tuples with velocity and description corresponding to the conditions
    Description = if
        Velocity == 0 -> {Velocity, 'stable'};
        Velocity >= 5, Velocity < 10 -> {Velocity, 'moving'};
        Velocity >= 10, Velocity < 20 -> {Velocity, 'fast'};
        Velocity >= 20 -> {Velocity, 'speedy'}
    end,

    if
        (Velocity > 40) -> 
            %% print side effect
            io:format("Look out below!~n");

        %% every if must find some true statement or it will
        %% report an error in those cases when nothing matches
        true -> true
    end,

    %% return tuple with velocity and description
    Description.
