-module(lock3).

-export([start/1]).

start(MyId) ->
    spawn(fun() -> init(MyId) end).

init(MyId) ->
    receive
        {peers, Nodes} ->
            open(Nodes, MyId, 0);
        stop ->
            ok
    end.

open(Nodes, MyId, GlobalClock) ->
    receive
        {take, Master, Ref} ->
            NewGlobalClock = GlobalClock + 1,
            MyRequestClock = NewGlobalClock,
            Refs = requests(Nodes, MyId, NewGlobalClock),
            wait(Nodes, Master, Refs, [], Ref, MyId, MyRequestClock, NewGlobalClock);
        {request, From, Ref, _FromId, FromRequestClock} ->
            NewGlobalClock = max(GlobalClock, FromRequestClock),
            From ! {ok, Ref},
            open(Nodes, MyId, NewGlobalClock);
        stop ->
            ok
    end.

requests(Nodes, MyId, GlobalClock) ->
    lists:map(fun(P) ->
                 R = make_ref(),
                 P ! {request, self(), R, MyId, GlobalClock},
                 R
              end,
              Nodes).

wait(Nodes, Master, [], Waiting, TakeRef, MyId, _MyRequestClock, GlobalClock) ->
    Master ! {taken, TakeRef},
    held(Nodes, Waiting, MyId, GlobalClock);
wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyRequestClock, GlobalClock) ->
    receive
        {request, From, Ref, FromId, FromRequestClock} ->
            NewGlobalClock = max(GlobalClock, FromRequestClock),
            if FromRequestClock < MyRequestClock or (FromRequestClock == MyRequestClock and (MyId > FromId)) ->
                   From ! {ok, Ref},
                   wait(Nodes, Master, Refs, Waiting, TakeRef, MyId, MyRequestClock, NewGlobalClock);
               true ->
                   wait(Nodes, Master, Refs, [{From, Ref} | Waiting], TakeRef, MyId, MyRequestClock, NewGlobalClock)
            end;
        {ok, Ref} ->
            NewRefs = lists:delete(Ref, Refs),
            wait(Nodes, Master, NewRefs, Waiting, TakeRef, MyId, MyRequestClock, GlobalClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, GlobalClock)
    end.

ok(Waiting) ->
    lists:map(fun({F, R}) -> F ! {ok, R} end, Waiting).

held(Nodes, Waiting, MyId, GlobalClock) ->
    receive
        {request, From, Ref, _FromId, FromRequestClock} ->
            NewGlobalClock = max(GlobalClock, FromRequestClock),
            held(Nodes, [{From, Ref} | Waiting], MyId, NewGlobalClock);
        release ->
            ok(Waiting),
            open(Nodes, MyId, GlobalClock)
    end.
