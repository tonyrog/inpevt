inpevt
======

Linux input event port driver and erlang app

# exploring buses and devices

## list current buses (from /proc/bus/input/devices)

    > inpevt:list_devices().
	
The return device list is on form [dev()] where dev()
is a map with various bus and device properties, like bus, vendor,
name, device ...
	
## match buses by bus type (also from proc)

	> inpevt:list_matched_devices([{bus, bluetooth}]).
	
## match buses by name (also from proc)

	> inpevt:list_matched_devices([{name, "Jabra"}]).

# Add devices to the inpevt server

## add matched input devices 

	> inpevt:add_matched_devices([{bus,usb},{name, "Jabra"}]).
	
## add one device

	> inpevt:add_device(#{ device=>"/dev/input/event5"}).

## View loaded devices

	> inpevt:get_devices().

The return device list similar to to list_devices but contain event
capabilities and port information (for subscriptions).

# Subscribe to events

## subscribe to all added devices

	> inptevt:subscribe().

or we can subscribe to matched devices

	> inptevt:subscribe_matched_devices([{capability, switch, headphone_insert}]).

After this we can look in the message box and see what we find.
If done from shell we can call flush().

	> flush().

	Shell got {input_event,#Ref<0.303950867.1630535681.233067>,1663625669,548470,
                       sw,headphone_insert,2,0}
	Shell got {input_event,#Ref<0.303950867.1630535681.233067>,1663625669,548470,
                       syn,report,0,0}
	ok
	> flush().
	Shell got {input_event,#Ref<0.303950867.1630535681.233067>,1663625679,824485,
                       sw,headphone_insert,2,1}
	Shell got {input_event,#Ref<0.303950867.1630535681.233067>,1663625679,824485,
                       syn,report,0,0}
	ok
