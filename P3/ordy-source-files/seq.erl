-module(seq).
-export([null/0, new/1, maxIncrement/2, max/2, lessthan/2]).

%%% Functions to handle the sequence numbers.
null() ->
    {0,0}.
    
new(Id) ->
    {0, Id}.
    
maxIncrement({Pn, Pi}, {Nn, _}) ->
    {erlang:max(Pn, Nn) + 1, Pi}.

max(P, N) ->
    case lessthan(P, N) of
        true ->
            N;
        false ->
            P
    end.

lessthan({Pn, Pi}, {Nn, Ni}) ->
    (Pn < Nn) or ((Pn == Nn) and (Pi < Ni)).
