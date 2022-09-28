%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

-module(inpevt).

-export([start/0, stop/0]).
-export([get_devices/0,get_matched_devices/1]).
-export([diff_devices/1, diff_devices/2]).
-export([subscribe_matched_devices/1]).
-export([subscribe/0, subscribe/1, unsubscribe/1]).
-export([add_devices/1, add_device/1, delete_devices/1, delete_device/1]).
-export([add_device_/1, delete_device_/1]).
%% proc devices
-export([list_devices/0, list_matched_devices/1]).
-export([add_matched_devices/1]).


%% -define(dbg(F,A), ok).
-define(dbg(F,A), io:format((F),(A))).
-define(SERVER, inpevt_server).

%% dev list as return from inpevt_proc_bus module
-type dev() :: #{ bus := atom(),
		  vendor := integer(),
		  product := integer(),
		  version := integer(),
		  name => string(),
		  handlers => [string()],
		  device := Filename::string(),
		  topology := string(),
		  sysfs := string(),
		  ev => string(),
		  sw => string(),
		  prop => string() 
		}.

%% dev list as return from inpevt_server
-type idev() :: #{ id := string(),
		   port := port(),
		   device := string(),
		   name := string(),
		   bus := atom(),
		   vendor := integer(),
		   product := integer(),
		   version := integer(),
		   topology := string(),
		   capabilities => [capa()]
		 }.
		   
		   

-type capa() :: {CapKey::atom(),
		 [{CapSpec::atom()}|
		  {CapSpec::atom(),integer()}]}.
%% ...

-type devmatch() :: 
	{capability,CapKey::atom()} |
	{capability,CapKey::atom(),CapSpec::atom()} |
	{Key::atom(), Value::term()|Pattern::string()} |
	Key::atom().

start() ->
    application:ensure_all_started(inpevt).

stop() ->
    gen_server:call(?SERVER, stop).

-spec get_devices() -> [idev()].
get_devices() ->
    get_matched_devices([]).

-spec get_matched_devices([devmatch()]) -> [idev()].
get_matched_devices(Match) ->
    gen_server:call(?SERVER, {get_devices, Match }).

-spec diff_devices(Saved::[idev()]) -> {Added::[idev()], Removed::[idev()]}.
diff_devices(Saved) ->
    diff_devices(list_devices(), Saved).

-spec diff_devices(Current::[dev()], Saved::[idev()]) ->
	  {Added::[dev()], Removed::[idev()]}.
diff_devices(Current, Saved) ->
    diff_devices_(Current, Saved, []).

diff_devices_([D=#{ device := Filename }|Ds], Saved, Added) ->
    case take_device(Filename, Saved) of
	false ->
	    diff_devices_(Ds, Saved, [D|Added]);
	{_S, Saved1} ->
	    diff_devices_(Ds, Saved1, Added)
    end;
diff_devices_([], Saved, Added) ->
    {Added, Saved}.

take_device(Filename, Ds) ->
    take_device_(Filename, Ds, []).
    
take_device_(Filename, [D=#{ device := Filename }|Ds], Acc) ->
    {D, Ds++Acc};
take_device_(Filename, [D|Ds], Acc) ->
    take_device_(Filename, Ds, [D|Acc]);
take_device_(_Filename, [], _Acc) ->
    false.

%% subscribe to all devices
-spec subscribe() ->
	  [{Ref::reference(),Dev::idev()}].
subscribe() ->
    subscribe(get_devices()).

%% subscribe to devices in device list (from get_devices etc)
-spec subscribe(D::idev()) ->
	  {Ref::reference(),Dev::idev()};
	       ([D::idev()]) -> 
	  [{Ref::reference(),Dev::idev()}].
subscribe(D=#{port := Port}) ->
    {ok,Ref} = subscribe_(Port),
    {Ref, D};
subscribe([D=#{port := Port}|Ds]) ->
    {ok,Ref} = subscribe_(Port),
    [{Ref,D} | subscribe(Ds)];
subscribe([]) ->
    [].

-spec subscribe_matched_devices(Match::[devmatch()]) ->
	  [{Ref::reference(),Dev::idev()}].

subscribe_matched_devices(Match) ->
    Ds = get_matched_devices(Match),
    lists:foldl(
      fun(D=#{ port := Port }, Acc) ->
	      {ok,Ref} = subscribe_(Port),
	      [{Ref,D}|Acc];
	 (_,Acc) ->
	      Acc
      end, [], Ds).

-spec subscribe_(Port::port()) ->
	  {ok,Ref::reference()} | {error, Reason::term()}.
subscribe_(Port) ->
    gen_server:call(?SERVER, {subscribe, Port, self() }).

-spec unsubscribe(Ref::reference()) -> ok.
unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe, Ref}).

%% add devices from a list
-spec add_devices([dev()]) -> [idev()].

add_devices([D|Ds]) ->
    case add_device(D) of
	[D1] -> [D1|add_devices(Ds)];
	[] -> add_devices(Ds)
    end;
add_devices([]) ->
    [].

-spec add_device(dev()) -> [idev()].
add_device(#{device := Filename}) ->
    case add_device_(Filename) of
	{ok, D1} -> [D1];
	{error,Reason} ->
	    ?dbg("unable to add device ~s ~p\n", [Filename, Reason]),
	    []
    end.

-spec add_device_(Filename::string()) -> {ok,idev()} |
	  {error, Reason::term()}.
add_device_(Filename) ->
    gen_server:call(?SERVER, {add_device, Filename }).

%% delete devices from a list
-spec delete_devices([D::dev()]) -> [dev()].

delete_devices([D|Ds]) ->
    case delete_device(D) of
	[D1] -> [D1 | delete_devices(Ds)];
	[] -> delete_devices(Ds)
    end;
delete_devices([]) ->
    [].

-spec delete_device(dev()) -> [] | [dev()].
delete_device(D=#{ device := Filename }) ->
    case delete_device_(Filename) of
	ok -> [D];
	{error,Reason} -> 
	    ?dbg("unable to delete device ~s ~p\n", [Filename, Reason]),
	    []
    end.


-spec delete_device_(Filename::string()) -> ok.
delete_device_(Filename) ->
    gen_server:call(?SERVER, {delete_device, Filename }).

-spec list_matched_devices(Match::[devmatch()]) -> [dev()].
list_matched_devices(Match) ->
    inpevt_proc_bus:match_devices(Match).

-spec list_devices() -> [dev()].

list_devices() ->
    inpevt_proc_bus:list_devices().

-spec add_matched_devices(Match::[devmatch()]) -> [idev()].

add_matched_devices(Match) ->
    lists:foldl(
      fun(#{ name := _Name, device := Filename }, Acc) ->
	      ?dbg("add device: ~s, file=~s\n", [_Name, Filename]),
	      case add_device_(Filename) of
		  {ok,D1} -> [D1|Acc];
		  {error,_Error} ->
		      ?dbg("error adding device ~p\n", [_Error]),
		      Acc
	      end;
	 (_, Acc) ->
	      Acc
      end, [], list_matched_devices(Match)).
