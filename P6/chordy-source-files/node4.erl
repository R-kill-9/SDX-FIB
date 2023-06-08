-module(node4).
-export([start/1, start/2]).

-define(Stabilize, 1000).
-define(Timeout, 5000).

start(MyKey) ->
    start(MyKey, nil).

start(MyKey, PeerPid) ->
    timer:start(),
    spawn(fun() -> init(MyKey, PeerPid) end).

init(MyKey, PeerPid) ->
    Predecessor = nil,
    {ok, Successor} = connect(MyKey, PeerPid),
    schedule_stabilize(),    
    Storage = storage:create(),
    Replica = storage:create(),
    node(MyKey, Predecessor, Successor, Storage, nil, Replica).
    %nil en next porque suponemos que está vacio

connect(MyKey, nil) ->
    {ok, {MyKey, nil, self()}};
connect(_, PeerPid) ->
    Qref = make_ref(),
    PeerPid ! {key, Qref, self()},
    receive
        {Qref, Skey} ->
            {ok, {Skey, monit(PeerPid), PeerPid}}
    after ?Timeout ->
        io:format("Timeout: no response from ~w~n", [PeerPid])
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

node(MyKey, Predecessor, Successor, Store, Next, Replica) ->
    receive 
        {key, Qref, Peer} ->
            Peer ! {Qref, MyKey},
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {notify, NewPeer} ->
            {NewPredecessor, NewStore} = notify(NewPeer, MyKey, Predecessor, Store),
            {_Skey, _Sref, Spid} = Successor,
            Spid ! {pushreplica, NewStore},
            node(MyKey, NewPredecessor, Successor, NewStore, Next, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {status, Pred, Nx} ->
            {NewSuccessor, NewNext} = stabilize(Pred, Nx, MyKey, Successor),
            node(MyKey, Predecessor, NewSuccessor, Store, NewNext, Replica);
        stabilize ->
            stabilize(Successor),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Added, Next, Replica);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            {_Skey, _Sref, Spid} = Successor,
            Spid ! {pushreplica, Merged},
            node(MyKey, Predecessor, Successor, Merged, Next, Replica);
        {'DOWN', Ref, process, _, _} ->
            {NewPred, NewSucc, NewNext, NewStore, NewReplica} = down(Ref, Predecessor, Successor, Next, Store, Replica),
            node(MyKey, NewPred, NewSucc, NewStore, NewNext, NewReplica);
        {replicate, Key, Value} ->
            NewReplica = storage:add(Key, Value, Replica),
            node(MyKey, Predecessor, Successor, Store, Next, NewReplica);
        {pushreplica, NewReplica} ->
            node(MyKey, Predecessor, Successor, Store, Next, NewReplica);
        stop ->
            ok;
        probe ->
            create_probe(MyKey, Successor, Store, Replica),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {probe, MyKey, Nodes, T} ->
            remove_probe(MyKey, Nodes, T),
            node(MyKey, Predecessor, Successor, Store, Next, Replica);
        {probe, RefKey, Nodes, T} ->
            forward_probe(MyKey, RefKey, [MyKey|Nodes], T, Successor, Store, Replica),
            node(MyKey, Predecessor, Successor, Store, Next, Replica)
   end. 

stabilize(Pred, Next, MyKey, Successor) ->
  {Skey, Sref, Spid} = Successor,
  case Pred of
      nil ->
        Spid ! {notify, {MyKey, self()}},
          {Successor, Next};
      {MyKey, _} ->
        {Successor, Next};
      {Skey, _} ->
          Spid ! {notify, {MyKey, self()}},
          {Successor, Next};
      {Xkey, Xpid} ->
            case key:between(Xkey, MyKey, Skey) of
                true ->
                    demonit(Sref),
                    Xref = monit(Xpid),
                    self() ! stabilize,
                    {{Xkey, Xref, Xpid}, {Skey, Spid}};
                false ->
                    Spid ! {notify, {MyKey, self()}},
                    {Successor, Next}
            end
    end.

stabilize({_, _Sref, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, {Skey, _Sref, Spid}) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, _Pref, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
        end.

add(Key, Value, Qref, Client, _, nil, {_, _Sref, Spid}, Store) ->
    Spid ! {add, Key, Value, Qref, Client},%% TODO: ADD SOME CODE
    Store;
add(Key, Value, Qref, Client, MyKey, {Pkey, _Pref, _}, {_, _Sref, Spid}, Store) ->
    case key:between(Key , Pkey , MyKey) of %% TODO: ADD SOME CODE
        true ->
            Added = storage:add(Key, Value, Store),  %% TODO: ADD SOME CODE
                Client ! {Qref, ok},
                Spid ! {replicate, Key, Value},
                Added;
        false ->
            Spid ! {add, Key, Value, Qref, Client},%% TODO: ADD SOME CODE
            Store
    end.

lookup(Key, Qref, Client, _, nil, {_, _Sref, Spid}, _) ->
    Spid ! {lookup, Key, Qref, Client};
lookup(Key, Qref, Client, MyKey, {Pkey, _Pref,  _}, {_, _Sref, Spid}, Store) ->
    case key:between(Key , Pkey , MyKey) of %% TODO: ADD SOME CODE
        true ->
            Result = storage:lookup(Key, Store) , %% TODO: ADD SOME CODE
            Client ! {Qref, Result};
        false ->
            Spid ! {lookup, Key, Qref, Client}%% TODO: ADD SOME CODE
    end.

handover(Store, MyKey, Nkey, Npid) ->
    {Keep, Leave} = storage:split(MyKey, Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Store, MyKey, Nkey, Npid),
            Nref = monit(Npid),
            {{Nkey, Nref, Npid}, Keep};
        {Pkey, Pref,  _} ->
            case key:between(Nkey, Pkey, MyKey) of
                true ->
                    Keep = handover(Store, MyKey, Nkey, Npid),
                    demonit(Pref),
                    Nref = monit(Npid),
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

monit(Pid) ->
    erlang:monitor(process, Pid).

demonit(nil) ->
    ok;
demonit(MonitorRef) ->
    erlang:demonitor(MonitorRef, [flush]).

down(Ref, {_, Ref, _}, {_Skey, _Sref, Spid}, Next, Store, Replica) ->
    NewStore = storage:merge(Store, Replica),
    NewReplica = storage:create(),
    Spid ! {pushreplica, NewStore},
    {nil, {_Skey, _Sref, Spid}, Next, NewStore, NewReplica};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}, Store, Replica) ->
    Nref = monit(Npid),
    self() ! stabilize,
    {Predecessor, {Nkey, Nref, Npid}, nil, Store, Replica}.

create_probe(MyKey, {_, _Sref, Spid}, Store, Replica) ->
    Spid ! {probe, MyKey, [MyKey], erlang:monotonic_time()},
    io:format("Node ~w created probe -> Store: ~w~n -> Replica: ~w~n", [MyKey, Store, Replica]).
	
remove_probe(MyKey, Nodes, T) ->
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2-T, native, microsecond),
    io:format("Node ~w received probe after ~w us -> Ring: ~w~n", [MyKey, Time, Nodes]).
	
forward_probe(MyKey, RefKey, Nodes, T, {_, _Sref, Spid}, Store, Replica) ->
    Spid ! {probe, RefKey, Nodes, T},
    io:format("Node ~w forwarded probe started by node ~w -> Store: ~w~n -> Replica: ~w~n", [MyKey, RefKey, Store, Replica]).
