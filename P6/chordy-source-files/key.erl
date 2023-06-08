-module(key).
-export([generate/0, between/3]).

-define(m, 30). % 30-bit identifiers

generate() ->
    rand:uniform(1 bsl ?m) - 1.

between(Key, From, To) ->
    if 
        From == To ->
            true;
        From < To ->
            (From < Key) and (Key =< To);
        From > To ->
            (From < Key) or (Key =< To)
    end.
