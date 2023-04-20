-module(groupy).
-export([start/2, stop/0]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(Module, Sleep) ->
    spawn('w1@127.0.0.1',fun()-> register(a, worker:start("P1", Module, Sleep))end),
    spawn('w2@127.0.0.1',fun()->register(b, worker:start("P2", Module, {a, 'w1@127.0.0.1'}, Sleep))end),
    spawn('w3@127.0.0.1',fun()->register(c, worker:start("P3", Module, {b, 'w2@127.0.0.1'}, Sleep))end),
    spawn('w4@127.0.0.1',fun()->register(d, worker:start("P4", Module, {c, 'w3@127.0.0.1'}, Sleep))end),
    spawn('w5@127.0.0.1',fun()->register(e, worker:start("P5", Module, {d, 'w4@127.0.0.1'}, Sleep))end).


stop() ->
    {a,'w1@127.0.0.1'} ! stop,
    {b, 'w2@127.0.0.1'} ! stop,
    {c, 'w3@127.0.0.1'} ! stop,
    {d, 'w4@127.0.0.1'} ! stop,
    {e, 'w5@127.0.0.1'} ! stop.