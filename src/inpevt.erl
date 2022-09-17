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

-export([get_devices/0,get_devices/1, get_devices/2,i/0]).
-export([subscribe/0, subscribe/2, unsubscribe/2]).
-export([add_device/1]).
-export([list_devices/0, match_devices/1]).
-export([add_matched_devices/1]).

get_devices() ->
    gen_server:call(inpevt_server, { get_devices }).

get_devices(Capability) ->
    gen_server:call(inpevt_server, { get_devices, Capability}).

get_devices(Capability, CapSpec) ->
    gen_server:call(inpevt_server, { get_devices, Capability, CapSpec}).

%% subscribe to all devices
subscribe() ->
    {ok,Ds} = get_devices(),
    lists:foreach(
      fun({device,Port,_Filename,_Sub,_Desc}) ->
	      subscribe(Port, self())
      end, Ds).

subscribe(Port, Pid) ->
    gen_server:call(inpevt_server, { subscribe, Port, Pid }).

unsubscribe(Port,Pid) ->
    gen_server:call(inpevt_server, { unsubscribe, Port, Pid}).

add_device(FileName) ->
    gen_server:call(inpevt_server, { add_device, FileName }).

i() ->
    gen_server:call(inpevt_server, { i }).



-define(BUS_PCI,	16#01).
-define(BUS_ISAPNP,	16#02).
-define(BUS_USB,	16#03).
-define(BUS_HIL,	16#04).
-define(BUS_BLUETOOTH,	16#05).
-define(BUS_VIRTUAL,	16#06).

-define(BUS_ISA,	16#10).
-define(BUS_I8042,	16#11).
-define(BUS_XTKBD,	16#12).
-define(BUS_RS232,	16#13).
-define(BUS_GAMEPORT,	16#14).
-define(BUS_PARPORT,	16#15).
-define(BUS_AMIGA,	16#16).
-define(BUS_ADB,	16#17).
-define(BUS_I2C,	16#18).
-define(BUS_HOST,	16#19).
-define(BUS_GSC,	16#1A).
-define(BUS_ATARI,	16#1B).
-define(BUS_SPI,	16#1C).
-define(BUS_RMI,	16#1D).
-define(BUS_CEC,	16#1E).
-define(BUS_INTEL_ISHTP,16#1F).

add_matched_devices(Match) ->
    lists:foreach(
      fun({Name,Attrs}) ->
	      case lists:keyfind(device, 1, Attrs) of
		  {device,FileName} ->
		      io:format("add device: ~s, file=~s\n", 
				[Name, FileName]),
		      add_device(FileName);
		  _ ->
		      ok
	      end
      end, match_devices(Match)).
	 
%% match devices among list_devices()
match_devices(Match) ->
    Ds = list_devices(),
    lists:filter(
      fun({_Name,Attrs}) ->
	      match_attrs(Match, Attrs)
      end, Ds).

match_attrs([{Key,Value}|Match], Attrs) -> %% value must match
    case lists:keyfind(Key, 1, Attrs) of
	{_,Value} -> match_attrs(Match, Attrs);
	_ -> false
    end;
match_attrs([Key|Match], Attrs) -> %% key must exist
    case lists:keymember(Key, 1, Attrs) of
	true -> match_attrs(Match, Attrs);
	false-> false
    end;
match_attrs([], _Attrs) ->
    true.

%% list device found in /proc/bus/input/devices
list_devices() ->
    {ok,Bin} = file:read_file("/proc/bus/input/devices"),
    collect_buses(binary:split(Bin, <<"\n">>, [global,trim_all]), []).

collect_buses([<<$I,$:,$\s,Data/binary>>|Lines], Acc) ->
    collect_bus(Lines, lists:reverse(scan_kv(Data)), Acc);
collect_buses([_|Lines], Acc) ->
    collect_buses(Lines, Acc);
collect_buses([], Acc) ->
    lists:reverse(Acc).

collect_bus([<<$N,$:,$\s,"Name=",Name/binary>>|Lines], Info, Acc) ->
    String = scan_string(Name),
    collect_bus(Lines, [{name,String}|Info], Acc);
collect_bus([<<$I,$:,$\s,Data/binary>>|Lines], Info, Acc) ->
    collect_bus(Lines, lists:reverse(scan_kv(Data)), [make_bus(Info)|Acc]);
collect_bus([<<_,$:,$\s,Data/binary>>|Lines], Info, Acc) ->
    case binary:split(Data, <<"=">>, [trim]) of
	[_K] -> collect_bus(Lines, Info, Acc);
	[K,V] ->
	    KV = {binary_to_atom(string:lowercase(K)),
		  binary_to_list(string:trim(V))},
	    collect_bus(Lines, [KV|Info], Acc)
    end;
collect_bus([],Info,Acc) ->
    lists:reverse([make_bus(Info)|Acc]).

make_bus(Info) ->
    {value,{_,Name},Info1} = lists:keytake(name, 1, Info),
    Info2 = add_devices(Info1),
    Info3 = add_bustype(Info2),
    {Name, lists:reverse(Info3)}.

add_bustype(Info) ->
    case lists:keyfind(bus, 1, Info) of
	{bus,Bus} ->
	    Type = case list_to_integer(Bus, 16) of
		       ?BUS_PCI -> pci;
		       ?BUS_ISAPNP -> isapnp;
		       ?BUS_USB -> usb;
		       ?BUS_HIL -> hil;
		       ?BUS_BLUETOOTH -> bluetooth;
		       ?BUS_VIRTUAL -> virtual;
		       ?BUS_ISA -> isa;
		       ?BUS_I8042 -> i8042;
		       ?BUS_XTKBD -> xtkbd;
		       ?BUS_RS232 -> rs232;
		       ?BUS_GAMEPORT -> gameport;
		       ?BUS_PARPORT -> parport;
		       ?BUS_AMIGA -> amiga;
		       ?BUS_ADB -> adb;
		       ?BUS_I2C -> i2c;
		       ?BUS_HOST -> host;
		       ?BUS_GSC -> gsc;
		       ?BUS_ATARI -> atari;
		       ?BUS_SPI -> spi;
		       ?BUS_RMI -> rmi;
		       ?BUS_CEC -> cec;
		       ?BUS_INTEL_ISHTP -> intel_ishtp;
		       _ -> unknown
		   end,
	    [{bus_type, Type}|Info];
	false ->
	    Info
    end.

%% insert {device,"/dev/input/eventX"} where possible
%% and update handlers as a list
add_devices(Info) ->
    case lists:keytake(handlers, 1, Info) of
	{value,{_,Handlers},Info2} ->
	    Hs = string:split(Handlers, " ", all),
	    case [E || E <- Hs, lists:prefix("event", E)] of
		[] -> 
		    [{handlers,Hs}|Info2];
		Events -> 
		    [{device,filename:join("/dev/input",E)} ||
			E <- Events]++[{handlers,Hs}|Info2]
	    end;
	false ->
	    Info
    end.
    


scan_string(<<$",Rest/binary>>) ->
    Size = byte_size(Rest),
    case Rest of
	<<String:(Size-1)/binary,$">> -> binary_to_list(String);
	String -> binary_to_list(String)
    end;
scan_string(String) -> String.
	      
scan_kv(Bin) ->
    lists:map(
      fun(KV) ->
	      [K,V] = binary:split(KV, <<"=">>, [trim]),
	      {binary_to_atom(string:lowercase(K)), 
	       binary_to_list(string:trim(V))}
      end, binary:split(Bin, <<" ">>, [global,trim_all])).
