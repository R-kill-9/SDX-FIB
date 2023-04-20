-module(worker).
-export([start/3, start/4]).

-define(change, 20).
-define(color, {0,0,0}).
-define(timeout, 1000).
-define(retries, 2).

start(Name, Module, Sleep) ->
    spawn(fun() -> init(Name, Module, Sleep) end).

init(Name, Module, Sleep) ->
    Cast = apply(Module, start, [Name]),
    Color = ?color,
    init_cont(Name, Cast, Color, Sleep).

start(Name, Module, Peer, Sleep) ->
    spawn(fun() -> init(Name, Module, Peer, Sleep) end).

init(Name, Module, Peer, Sleep) ->
    Cast = apply(Module, start, [Name, Peer]),
    receive
        joined ->
            Color = state_transfer(Name, Cast, ?retries),
            if Color /= stop ->
                 init_cont(Name, Cast, Color, Sleep),
                 Cast ! stop;
               true ->
                 io:format("worker ~s: join failed: state transfer: ABORT~n", [Name]),
                 Cast ! stop
            end;
        {error, Error} ->
            io:format("worker ~s: join failed: ~s: ABORT~n", [Name, Error]);
        stop -> 
            ok
    end.

state_transfer(_, _, 0) ->
    stop;
state_transfer(Name, Cast, Retries) ->
    Ref = make_ref(),
    Cast ! {mcast, {state_req, Ref}},
    Tref = erlang:send_after(?timeout, self(), timeout),
    Result = state_transfer_recv(Name, Cast, Retries, Ref),
    erlang:cancel_timer(Tref),
    Result.

state_transfer_recv(Name, Cast, Retries, Ref) ->
    receive
        {deliver, {state_req, Ref}} ->
            receive
                {deliver, {set_state, Ref, Color}} ->
                    Color;
                {join, Peer} ->
                    Cast ! {join, Peer},
                    state_transfer_recv(Name, Cast, Retries, Ref);
                stop -> 
                    stop;
                timeout ->
                    state_transfer(Name, Cast, Retries-1)
            end;
        {join, Peer} ->
            Cast ! {join, Peer},
            state_transfer_recv(Name, Cast, Retries, Ref);
        stop -> 
            stop;
        timeout ->
            state_transfer(Name, Cast, Retries-1);
        _Ignore ->
            state_transfer_recv(Name, Cast, Retries, Ref)
    end.

init_cont(Name, Cast, Color, Sleep) ->
    Gui = gui:start(Name, self()),
    Gui ! {color, Color}, 
    Wait = if Sleep == 0 -> 0; true -> rand:uniform(Sleep) end,
    erlang:send_after(Wait, self(), cast_change),
    worker(Name, Cast, Color, Gui, Sleep),
    Gui ! stop.

worker(Name, Cast, Color, Gui, Sleep) ->
    receive
        {deliver, {change_state, N}} ->
            NewColor = change_color(N, Color),
            Gui ! {color, NewColor},
            worker(Name, Cast, NewColor, Gui, Sleep);
        {deliver, {state_req, Ref}} ->
            Cast ! {mcast, {set_state, Ref, Color}},
            worker(Name, Cast, Color, Gui, Sleep);
        {deliver, {set_state, _, _}} ->
            worker(Name, Cast, Color, Gui, Sleep);
        {join, Peer} ->
            Cast ! {join, Peer},
            worker(Name, Cast, Color, Gui, Sleep);
        cast_change ->
            Cast !  {mcast, {change_state, rand:uniform(?change)}},
            Wait = if Sleep == 0 -> 0; true -> rand:uniform(Sleep) end,
            erlang:send_after(Wait, self(), cast_change),
            worker(Name, Cast, Color, Gui, Sleep);
        stop ->
            Cast ! stop,
            ok;
        Error ->
            io:format("worker ~s: strange message: ~w~n", [Name, Error]),
            worker(Name, Cast, Color, Gui, Sleep)
    end.

change_color(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
