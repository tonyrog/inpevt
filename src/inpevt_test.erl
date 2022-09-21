%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Using uinput_test detect virtual keyboard/mouse and capture input
%%% @end
%%% Created : 21 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(inpevt_test).

-export([test_keyboard/0]).
-export([test_mouse/0]).

-include("../include/inpevt.hrl").

%% using udev to detect when new keyboard/muse devnodes are created

test_keyboard() ->
    inpevt:start(),
    %% wait for Keyboard beeing defined using udef
    spawn(fun() ->
		  timer:sleep(100),
		  Cmd = filename:join(code:priv_dir(inpevt),"uinput_test") ++ 
		      " keyboard",
		  io:format("execute: ~p\n", [Cmd]),
		  os:cmd(Cmd)
	  end),
    ok = wait_udev("Test Keyboard", "feed/1111"),
    %% wait for /dev/input to be created (should wait for devnode!)
    timer:sleep(1000),
    [Dev0] = inpevt:list_matched_devices([{name, "Test Keyboard"}]),
    io:format("found matched device ~p\n", [Dev0]),
    [Dev1] = inpevt:add_matched_devices([{name, "Test Keyboard"}]),
    io:format("added matched device ~p\n", [Dev1]),
    [{Ref,_iDev}] = inpevt:subscribe(),
    io:format("subscribed ~p\n", [Ref]),
    Res = wait_events(Ref, 4000, []),
    inpevt:unsubscribe(Ref),
    inpevt:delete_device(Dev1),
    Res.

test_mouse() ->
    inpevt:start(),
    %% wait for Keyboard beeing defined using udef
    spawn(fun() ->
		  timer:sleep(100),
		  Cmd = filename:join(code:priv_dir(inpevt),"uinput_test") ++ 
		      " mouse",
		  io:format("execute: ~p\n", [Cmd]),
		  os:cmd(Cmd)
	  end),
    ok = wait_udev("Test Mouse", "feed/2222"),
    %% wait for /dev/input to be created (should wait for devnode!)
    timer:sleep(1000),
    [Dev0] = inpevt:list_matched_devices([{name, "Test Mouse"}]),
    io:format("found matched device ~p\n", [Dev0]),
    [Dev1] = inpevt:add_matched_devices([{name, "Test Mouse"}]),
    io:format("added matched device ~p\n", [Dev1]),
    [{Ref,_iDev}] = inpevt:subscribe(),
    io:format("subscribed ~p\n", [Ref]),
    Res = wait_events(Ref, 4000, []),
    inpevt:unsubscribe(Ref),
    inpevt:delete_device(Dev1),
    Res.

wait_events(Ref, Tmo, Acc) ->
    receive
	#input_event{id=Ref, type=syn,code_sym=report} ->
	    case Acc of
		[{key,enter,_,0}] -> lists:reverse(Acc);
		_ -> wait_events(Ref,Tmo,Acc)
	    end;
	#input_event{id=Ref, type=Type,code_sym=Sym,code_num=Num,value=Value} ->
	    wait_events(Ref, Tmo, [{Type,Sym,Num,Value}|Acc]);
	Other ->
	    io:format("GOT ~p\n", [Other]),
	    wait_events(Ref, Tmo, Acc)
    after Tmo ->
	    lists:reverse(Acc)
    end.


wait_udev(NAME, PRODUCT) ->
    Udev = udev:new(),
    Mon = udev:monitor_new_from_netlink(Udev, udev),
    %% add match tags...
    ok = udev:monitor_enable_receiving(Mon),
    wait_udev_loop(Udev, Mon, "add", NAME, PRODUCT, 5000).

wait_udev_loop(Udev, Mon, Action, NAME, PRODUCT, Tmo) ->
    Ref = erlang:make_ref(),
    case udev:select(Mon, Ref) of    
	select ->
	    receive
		{select, Mon, Ref, ready_input} ->
		    case udev:monitor_receive_device(Mon) of
			undefined ->
			    wait_udev_loop(Udev,Mon,Action,NAME,PRODUCT,Tmo);
			Dev ->
			    case udev:device_get_action(Dev) of
				Action ->
				    case match_device(Dev, NAME, PRODUCT) of
					true ->
					    ok;
					false ->
					    wait_udev_loop(Udev,Mon,Action,
							   NAME,PRODUCT,Tmo)
				    end;
				OtherAction ->
				    io:format("action=~p\n", [OtherAction]),
				    wait_udev_loop(Udev,Mon,Action,
						   NAME,PRODUCT,Tmo)
			    end
		    end
	    after Tmo ->
		    timeout
	    end;
	Error ->
	    Error
    end.

%% match property NAME and PRODUCT in udev

match_device(Dev, NAME, PRODUCT) ->
    Product = udev:device_get_property_value(Dev, "PRODUCT"),
    io:format("PRODUCT = ~p\n", [Product]),
    case re:run(Product, PRODUCT) of
	{match, _} -> 
	    Name = udev:device_get_property_value(Dev, "NAME"),
	    io:format("NAME = ~p\n", [Name]),
	    case re:run(Name, NAME) of
		{match, _} -> true;
		_ -> false
	    end;
	_ -> false
    end.
