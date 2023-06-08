-module(storage).
-export([create/0, add/3, lookup/2, merge/2, split/3]).

create() ->
  [].

add(Key, Value, Store) ->
  Present = lists:keymember(Key, 1, Store),
  if Present ->
       io:format("[Store:Add] Key ~w already exists. Replacing ...~n", [Key]),
       lists:keystore(Key, 1, Store, {Key, Value});
     true -> 
       lists:keystore(Key, 1, Store, {Key, Value})
  end.

lookup(Key, Store) ->
  case lists:keyfind(Key, 1, Store) of
    {Key, Value} ->
      Value;
    false ->
      io:format("[Store:Lookup] Key ~w does not exist~n", [Key]),
      false
  end.
		
merge(Store1, Store2) ->
  lists:keymerge(1, lists:keysort(1, Store1), lists:keysort(1, Store2)).

split(MyKey, NewKey, Store) ->
  lists:partition(fun({K, _}) -> 
                    key:between(K, NewKey, MyKey) 
                  end, Store).
