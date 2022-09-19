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
-export([get_devices/0,get_devices/1, get_devices/2,i/0]).
-export([subscribe/0, subscribe/2, unsubscribe/2]).
-export([add_device/1]).
%% proc devices
-export([list_devices/0, match_devices/1]).
-export([add_matched_devices/1]).

%% -define(dbg(F,A), ok).
-define(dbg(F,A), io:format((F),(A))).
-define(SERVER, inpevt_server).

start() ->
    application:ensure_all_started(inpevt).

stop() ->
    gen_server:call(?SERVER, stop).

get_devices() ->
    gen_server:call(?SERVER, { get_devices }).

get_devices(Capability) ->
    gen_server:call(?SERVER, { get_devices, Capability}).

get_devices(Capability, CapSpec) ->
    gen_server:call(?SERVER, { get_devices, Capability, CapSpec}).

%% subscribe to all devices
subscribe() ->
    {ok,Ds} = get_devices(),
    lists:foreach(
      fun({_Name,#{ port := Port }}) ->
	      subscribe(Port, self());
	 (_) ->
	      ok
      end, Ds).

subscribe(Port, Pid) ->
    gen_server:call(inpevt_server, { subscribe, Port, Pid }).

unsubscribe(Port,Pid) ->
    gen_server:call(inpevt_server, { unsubscribe, Port, Pid}).

add_device(FileName) ->
    gen_server:call(inpevt_server, { add_device, FileName }).

i() ->
    gen_server:call(inpevt_server, { i }).

match_devices(Match) ->
    inpevt_proc_bus:match_devices(Match).

list_devices() ->
    inpevt_proc_bus:list_devices().

% return list of device names opened
add_matched_devices(Match) ->
    lists:foldl(
      fun({Name,#{ device := Filename }}, Acc) ->
	      ?dbg("add device: ~s, file=~s\n", [Name, Filename]),
	      case add_device(Filename) of
		  {ok,_Dev} -> [Name|Acc];
		  {error,_Error} ->
		      ?dbg("error adding device ~p\n", [_Error]),
		      Acc
	      end;
	 (_, Acc) ->
	      Acc
      end, [], match_devices(Match)).
