%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Using uinput_test detect virtual keyboard/mouse and capture input
%%% @end
%%% Created : 21 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(inpevt_test).

-export([keyboard/0]).
-export([test_keyboard/0, test_keyboard/1]).
-export([test_mouse/0, test_mouse/1]).

-include("../include/inpevt.hrl").

%% wait for keyboard open and print events
keyboard() ->
    inpevt:start(),
    udev_monitor:start(
      [{subsystem, "input"},
       {tag, "power-switch"},
       {name, "UInput Keyboard"}
      ],
      fun add_remove_inpevt/4).

%% using udev to detect when new keyboard/muse devnodes are created
test_keyboard() -> test_keyboard(4000).
test_keyboard(Tmo) ->
    inpevt:start(),
    %% wait for Keyboard beeing defined using udef
    spawn(fun() ->
		  timer:sleep(100),
		  Cmd = filename:join(code:priv_dir(inpevt),"uinput_test") ++ 
		      " keyboard",
		  io:format("execute: ~p\n", [Cmd]),
		  os:cmd(Cmd)
	  end),
    udev_monitor:start(
      [{subsystem, "input"},
       {tag, "power-switch"},
       {name, "UInput Keyboard"},
       {timeout, Tmo}
      ],
      fun add_remove_inpevt/4).

test_mouse() -> test_mouse(4000).
test_mouse(Tmo) ->
    inpevt:start(),
    %% wait for Keyboard beeing defined using udef
    spawn(fun() ->
		  timer:sleep(100),
		  Cmd = filename:join(code:priv_dir(inpevt),"uinput_test") ++ 
		      " mouse",
		  io:format("execute: ~p\n", [Cmd]),
		  os:cmd(Cmd)
	  end),
    udev_monitor:start(
      [{subsystem, "input"},
       {name, "UInput Mouse"},
       {timeout, Tmo}
      ],
      fun add_remove_inpevt/4).


add_remove_inpevt("add", Info, _Dev, State) ->
    case proplists:get_value(devnode, Info, undefined) of
	undefined -> State;
	DevNode when is_list(DevNode) ->
	    case inpevt:add_device(#{device => DevNode}) of
		[] ->
		    io:format("unable to open ~p\n", [DevNode]),
		    State;
		[Added] ->
		    case inpevt:subscribe(Added) of
			{_Ref,_DevN} ->
			    State1 = start_timer(State),
			    io:format("devnode => ~p\n", [_DevN]),
			    State1#{ DevNode => Added };
			Res ->
			    io:format("error: ~p\n", [Res]),
			    State
		    end
	    end
    end;
add_remove_inpevt("remove", Info, _Dev, State) ->
    DevNode = proplists:get_value(devnode, Info, ""),
    case maps:take(DevNode, State) of
	error -> State;
	{D, State1} ->
	    inpevt:delete_device(D),
	    State1
    end;
add_remove_inpevt("event", _Info, Event, State) ->
    io:format("event: ~p\n", [Event]),
    case Event of
	stop -> {stop, Event};
	_ -> State
    end.


start_timer(State=#{ opts := Opts }) ->
    case proplists:get_value(timeout,Opts,infinity) of
	infinity -> State;
	Tmo ->
	    io:format("start timer ~w ms\n", [Tmo]),
	    TRef = erlang:send_after(Tmo, self(), stop),
	    State#{ timer => TRef }
    end.

