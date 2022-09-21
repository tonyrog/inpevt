%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Get device list from /proc/bus/input/devices
%%% @end
%%% Created : 18 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(inpevt_proc_bus).

-export([list_devices/0, match_devices/1]).

-include("../include/inpevt.hrl").

%% match devices among list_devices()
match_devices(Match) ->
    Ds = list_devices(),
    lists:filter(
      fun(Desc) ->
	      match_attrs(Match,Desc)
      end, Ds).

match_attrs([{Key,Pattern}|Match], Desc) when is_atom(Key) ->
    case maps:get(Key, Desc, undefined) of
	Pattern -> match_attrs(Match, Desc); %% exackt match
	Value when is_list(Value), is_list(Pattern) ->
	    case re:run(Value, Pattern) of %% try regexp
		nomatch -> false;
		{match,_} -> match_attrs(Match, Desc)
	    end;
	_ -> false
    end;
match_attrs([Key|Match], Desc) when is_atom(Key) -> %% key must exist
    case maps:is_key(Key, Desc) of    
	true -> match_attrs(Match, Desc);
	false-> false
    end;
match_attrs([], _Desc) ->
    true.

%% list device found in /proc/bus/input/devices
list_devices() ->
    {ok,Bin} = file:read_file("/proc/bus/input/devices"),
    collect_buses(binary:split(Bin, <<"\n">>, [global,trim_all]), []).

collect_buses([<<$I,$:,$\s,Data/binary>>|Lines], Acc) ->
    collect_bus(Lines, scan_kv(Data), Acc);
collect_buses([_|Lines], Acc) ->
    collect_buses(Lines, Acc);
collect_buses([], Acc) ->
    lists:reverse(Acc).

collect_bus([<<$N,$:,$\s,"Name=",Name/binary>>|Lines], Desc, Acc) ->
    String = scan_string(Name),
    collect_bus(Lines, Desc#{name=>String}, Acc);
collect_bus([<<$I,$:,$\s,Data/binary>>|Lines], Desc, Acc) ->
    collect_bus(Lines, scan_kv(Data), [make_bus(Desc)|Acc]);
collect_bus([<<_,$:,$\s,Data/binary>>|Lines], Desc, Acc) ->
    case binary:split(Data, <<"=">>, [trim]) of
	[_K] -> collect_bus(Lines, Desc, Acc);
	[K,V] ->
	    Key = binary_to_atom(string:lowercase(K)),
	    Value = binary_to_list(string:trim(V)),
	    collect_bus(Lines, add_kv(Key, Value, Desc), Acc)
    end;
collect_bus([],Desc,Acc) ->
    lists:reverse([make_bus(Desc)|Acc]).

make_bus(Desc=#{ name := _Name}) -> add_devices(Desc).

%% insert {device,"/dev/input/eventX"} where possible
%% and update handlers as a list
add_devices(Desc=#{ handlers := Handlers}) ->
    Hs = string:split(Handlers, " ", all),
    case [E || E <- Hs, lists:prefix("event", E)] of
	[] -> 
	    Desc#{handlers => Hs};
	[Event|_] -> %% only the first event, right now !
	    Desc#{handlers => Hs,
		  device=>filename:join("/dev/input",Event) }
    end.

scan_string(<<$",Rest/binary>>) ->
    Size = byte_size(Rest),
    case Rest of
	<<String:(Size-1)/binary,$">> -> binary_to_list(String);
	String -> binary_to_list(String)
    end;
scan_string(String) -> String.
	      
scan_kv(Bin) ->
    lists:foldl(
      fun(KV,Desc) ->
	      [K,V] = binary:split(KV, <<"=">>, [trim]),
	      add_kv(binary_to_atom(string:lowercase(K)),
		     binary_to_list(string:trim(V)), Desc)
      end, #{}, binary:split(Bin, <<" ">>, [global,trim_all])).

add_kv(bus, Bus, Desc) ->
    Bus1 = try list_to_integer(Bus, 16) of
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
	       B -> B
	   catch
	       error:_ -> Bus
	   end,
    Desc#{ bus=>Bus1 };
add_kv(vendor,Value,Desc) ->
    Desc#{ vendor=> hex(Value)};
add_kv(product,Value,Desc) ->
    Desc#{product => hex(Value) };
add_kv(version, Value, Desc) ->
    Desc#{version => hex(Value) };
add_kv(phys, Value, Desc) ->
    Desc#{topology => Value};
add_kv(Key, Value, Desc) when is_atom(Key) ->
    Desc#{Key => Value}.

hex(Value) ->
    try list_to_integer(Value, 16) of
	V -> V
    catch
	error:_ -> Value
    end.

	     

