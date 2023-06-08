-module(cache).

-export([lookup/2, add/3, remove/2]).

lookup(Name, Cache) -> 
	Instant = erlang:monotonic_time(),
	ExpirationTime = erlang:convert_time_unit(Instant, native, second),
    case lists:keyfind(Name, 1, Cache) of
    	false -> unknown;
    	{_, Expire, Reply} -> 
    		if 
    			Expire < ExpirationTime ->
    				invalid;
    				
    			true -> Reply
    		end
    end.

add(Domain, Expire, Reply, NewCache) ->
    lists:keystore(Domain, 1, NewCache, {Domain, Expire, Reply}).

remove(Name, Cache) ->
    lists:keydelete(Name, 1, Cache).


purge(Cache) ->
    Instant = erlang:convert_time_unit(erlang:monotonic_time(), native, second),
    lists:filter(fun({_Name, Expiration, _Entry}) -> Instant < Expiration end, Cache).