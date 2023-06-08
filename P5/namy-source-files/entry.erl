-module(entry).

-export([lookup/2, add/3, remove/2]).

lookup(Name, Entries) ->
    case lists:keyfind(Name, 1, Entries) of
        {Name, Entry} ->
            Entry;
        false ->
            unknown
    end.

add(Name, Entry, Entries) ->
    lists:keystore(Name, 1, Entries, {Name, Entry}).

remove(Name, Entries) ->
    lists:keydelete(Name, 1, Entries).