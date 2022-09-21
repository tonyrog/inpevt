%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2012 by magnus <magnus@feuerlabs.com>
%%%-------------------------------------------------------------------
-module(inpevt_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-include("../include/inpevt.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

-record(device,
	{
	 port  :: port(),
	 fname :: string(),
	 dsubs :: #{ reference() => pid() },
	 desc  :: #{ atom() => term() }
	}).

-record(state,
	{
	 devices = [] ::[#device{}],
	 devmap  = #{} :: #{ reference() => port()}
	}).

-define (IEDRV_CMD_MASK, 16#0000000F).
-define (IEDRV_CMD_OPEN, 16#00000001).
-define (IEDRV_CMD_CLOSE, 16#00000002).
-define (IEDRV_CMD_PROBE, 16#00000003).

-define (IEDRV_RES_OK, 0).
-define (IEDRV_RES_IO_ERROR, 1).
-define (IEDRV_RES_NOT_OPEN, 2).
-define (IEDRV_RES_ILLEGAL_ARG, 3).
-define (IEDRV_RES_COULD_NOT_OPEN, 4).

-define (INPEVT_DRIVER, "inpevt_driver").
-define (INPEVT_DIRECTORY, "/dev/input").
-define (INPEVT_PREFIX, "event").
-define (INPEVT_TOUCHSCREEN, "touchscreen").

-type port_result():: ok|{error, illegal_arg | io_error | not_open}.
-spec activate_event_port(port()) -> port_result().

-spec deactivate_event_port(port()) -> port_result().

-spec add_subscriber(pid(), #device {}, [#device{}]) ->
                            { ok, [#device{}] } |
                            {error, illegal_arg | io_error | not_open }.

-spec delete_subscription_from_device(reference(), #device{} ) -> #device{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Res = erl_ddll:load(code:priv_dir(inpevt), ?INPEVT_DRIVER),
    case Res of
	 ok -> 
	    {ok, #state{}};
	{ _error, Error } ->
	    { stop, Error };
	_ ->
	    { stop, unknown }
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Call, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({subscribe, Port, Pid}, _From, State) ->
    case lists:keytake(Port, #device.port, State#state.devices) of
        {value, Device, Ds1} ->
            case add_subscriber(Pid, Device, Ds1) of
                {ok, Ref, Ds2} ->
		    DevMap = maps:put(Ref, Port, State#state.devmap),
		    {reply, {ok,Ref}, State#state { devices=Ds2,devmap=DevMap}};
                { error, ErrCode } -> 
		    {reply, { error, ErrCode }, State }
            end;
        false -> 
	    {reply,{ error, not_found }, State}
    end;

handle_call({unsubscribe, Ref}, _From, State) ->
    case maps:take(Ref, State#state.devmap) of
	error ->
	    %% just ignore old? subscriptions
	    {reply, ok, State};
	{Port,DevMap} ->
	    Ds = delete_subscription_from_port(Port, Ref, State#state.devices),
	    {reply, ok, State#state { devices = Ds, devmap = DevMap } }
    end;

handle_call({get_devices, Match}, _From, State) ->
    DevList = match_devices(Match, State#state.devices),
    {reply, DevList, State};

handle_call({add_device, FileName}, _From, State) ->
    { Res, NewState } = add_new_device(FileName, State),
    { reply, Res, NewState };

handle_call({delete_device, FileName }, _From, State) ->
    { reply, ok, delete_device(FileName, State) };

handle_call(stop, _From, State) ->
    { stop, normal, ok, State }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Event=#input_event {}, State) ->
    dispatch_event(Event, State),
    {noreply, State};
handle_info({'DOWN', Ref, process, _Pid, _ }, State) ->
    case maps:take(Ref, State#state.devmap) of
	error ->
	    {noreply, State};
	{Port,DevMap} ->
	    Ds = delete_subscription_from_port(Port, Ref, State#state.devices),
	    {noreply, State#state { devices = Ds, devmap = DevMap }}
    end;
handle_info({'EXIT',Port,_Reason}, State) when is_port(Port) ->
    %% Check that this was a port we closed?
    {noreply, State};
handle_info(_Info, State) ->
    io:format("got info: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_return_value(Bits) ->
    if Bits =:= ?IEDRV_RES_OK -> ok;
       Bits =:= ?IEDRV_RES_ILLEGAL_ARG -> {error,illegal_arg};
       Bits =:= ?IEDRV_RES_IO_ERROR -> {error,io_error};
       Bits =:= ?IEDRV_RES_NOT_OPEN -> {error,not_open};
       Bits =:= ?IEDRV_RES_COULD_NOT_OPEN -> {error,not_open};
       true -> {error,unknown}
    end.

probe_event_file(Path, State) ->
    DeviceList = State#state.devices,
    Port = open_port({spawn, ?INPEVT_DRIVER}, []),
    { Res, ReplyID } = event_port_control(Port, ?IEDRV_CMD_PROBE, [Path]),
    case Res of
        ok ->
            receive
                { device_info, _, ReplyID, DrvDev } ->
		    Desc = make_desc(DrvDev,Port,Path),
		    NewDevice = #device { port=Port,
					  fname=Path,
					  dsubs=#{},
					  desc=Desc },
		    Ds =  [ NewDevice | DeviceList ],
		    {{ok,Desc},State#state { devices = Ds}}
            end;
        Error = {error,_} ->
            {Error, State#state { devices = DeviceList }}
    end.

make_desc(DrvDev,Port,Path) ->
    #{
      id => DrvDev#drv_dev_id.id,
      port => Port,
      device => Path,
      name => DrvDev#drv_dev_id.name,
      bus => DrvDev#drv_dev_id.bus,
      vendor => DrvDev#drv_dev_id.vendor,
      product => DrvDev#drv_dev_id.product,
      version => DrvDev#drv_dev_id.version,
      topology => DrvDev#drv_dev_id.topology,
      capabilities => DrvDev#drv_dev_id.capabilities
     }.    

%%
%% First subscriber added to a device will activate the event port, thus starting the
%% generation of devices.
%%
add_subscriber(Pid, Device, DevList) ->
    case maps:size(Device#device.dsubs) of
	0 ->
	    case activate_event_port(Device#device.port) of
		ok ->
		    add_additional_subscribers(Pid, Device, DevList);
		Error ->
		    Error
	    end;
	_ ->
	    add_additional_subscribers(Pid, Device, DevList)
    end.

add_additional_subscribers(Pid, Device, DevList) ->
    Ref = erlang:monitor(process, Pid),
    DSubs = Device#device.dsubs,
    Device1 = Device#device { dsubs = DSubs#{ Ref => Pid } },
    {ok, Ref, [Device1 | DevList] }.

delete_subscription_from_port(Port, Ref, Ds) ->
    case lists:keytake(Port, #device.port, Ds) of
	{value,Device,Ds1} ->
	    [delete_subscription_from_device(Ref, Device) | Ds1];
        false ->
            Ds
    end.

delete_subscription_from_device(Ref, Device) ->
    case maps:take(Ref, Device#device.dsubs) of
	error -> Device;
	{_Pid, DSubs} ->
	    demonitor(Ref, [flush]),
	    case maps:size(DSubs) of
		0 ->
		    ok = deactivate_event_port(Device#device.port);
		_ ->
		    ok
	    end,
	    Device#device { dsubs = DSubs }
    end.

activate_event_port(Port) ->
    { Res, _ReplyID } = event_port_control(Port, ?IEDRV_CMD_OPEN, []),
    Res.

deactivate_event_port(Port) ->
    {Res, _ReplyID } = event_port_control(Port, ?IEDRV_CMD_CLOSE, []),
    Res.

event_port_control(Port, Command, PortArg) ->
    ResList = port_control(Port, Command, PortArg),
    <<ResNative:32/native>> = list_to_binary(ResList),
    Res = convert_return_value(ResNative bsr 24),
    ReplyID = ResNative band 16#00FFFFFF,
    { Res, ReplyID }.

match_devices(Match, Devices) ->
    lists:foldl(
      fun(Dev, Acc) ->
	      Desc = Dev#device.desc,
	      case match(Match, Desc) of
		  true -> [Desc|Acc];
		  false -> Acc
	      end
      end, [], Devices).

%% Match capabilities and attributes in dev list
match([{capability,CapKey}|Match], Dev=#{ capabilities := Capabilities}) ->
    case proplists:is_defined(CapKey, Capabilities) of
	true -> match(Match, Dev);
	false -> false
    end;
match([{capability,CapKey,CapSpec}|Match],
      Dev=#{ capabilities := Capabilities}) ->
    case proplists:get_value(CapKey, Capabilities) of
	undefined -> false;
	CapValue ->
	    case proplists:is_defined(CapSpec, CapValue) of
		true -> match(Match, Dev);
		false -> false
	    end
    end;
match([{Key,Pattern}|Match], Dev) ->
    case maps:get(Key, Dev, undefined) of
	Pattern -> match(Match, Dev);
	Value when is_list(Value), is_list(Pattern) ->
	    case re:run(Value, Pattern) of %% try regexp
		nomatch -> false;
		{match,_} -> match(Match, Dev)
	    end;	
	_ -> false
    end;
match([Key|Match], Dev) ->
    case maps:is_key(Key, Dev) of
	true -> match(Match, Dev);
	false -> false
    end;
match([], _Dev) ->
    true.


dispatch_event(Event, State) ->
    case lists:keyfind(Event#input_event.id, #device.port,
		       State#state.devices) of
        false ->
            not_found;
        Device ->
            maps:foreach(
	      fun(Ref,Pid) ->
		      Pid ! Event#input_event{id=Ref}
	      end, Device#device.dsubs),
            found
    end.

add_new_device(Path, State) ->
    case lists:keyfind(Path, #device.fname, State#state.devices) of
        false ->
	    probe_event_file(Path, State);
        Dev ->
	    {{ok,Dev#device.desc},State}
    end.

delete_device(Path, State) ->
    case lists:keytake(Path,#device.fname,State#state.devices) of
        false ->
            State;
        %% We found the device. Close its port and remove it from state.
        {value, Device, NewDeviceList } ->
            maps:foreach(
	      fun(Ref,Pid) ->
		      demonitor(Ref, [flush]),
		      Pid ! 
			  #input_event { id = Ref,
					 sec = 0,
					 usec = 0,
					 type = removed,
					 code_sym = removed,
					 code_num = 0,
					 value = 0
				       }
	      end, Device#device.dsubs),
	    DevMap = maps:fold(
		       fun(Ref,_Pid,Map) ->
			       maps:remove(Ref, Map)
		       end, State#state.devmap, Device#device.dsubs),
            deactivate_event_port(Device#device.port),
            port_close(Device#device.port),
            State#state { devices = NewDeviceList, devmap=DevMap }
    end.
