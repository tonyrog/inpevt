inpevt
======

Linux input event port driver and erlang app

# exploring buses and devices

## list current buses (from /proc/bus/input/devices)

    > inpevt:list_devices().
	
The return device list is on form [{Name, Desc}] where Desc
is a map with various bus and device properties, like bus, vendor,
name, device ...
	
## match buses by bus type (also from proc)

	> inpevt:match_devices([{bus, bluetooth}]).
	
## match buses by name (also from proc)

	> inpevt:match_devices([{name, "Jabra"}]).

# Add devices to the inpevt server

## add matched input devices 

	> inpevt:add_matched_devices([{bus,usb},{name, "Jabra"}]).
	
## add device

	> inpevt:add_device("/dev/input/event5").

## View loaded devices

	> inpevt:get_devices().

The return device list similar to to list_devices but contain event
capabilities and port information (for subscriptions).

# Subscribe to events

## subscribe to all added devices

	> inptevt:subscribe().

After this we can look in the message box and see what we find.
If done from shell we can call flush().

	> flush().
	Shell got {input_event,#Port<0.6>,1663512313,570728,key,playcd,200,1}
	Shell got {input_event,#Port<0.6>,1663512313,570728,syn,report,0,0}
		Shell got {input_event,#Port<0.6>,1663512313,578297,key,playcd,200,0}
	Shell got {input_event,#Port<0.6>,1663512313,578297,syn,report,0,0}
