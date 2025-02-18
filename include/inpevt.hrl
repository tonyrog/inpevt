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
%%% Created : 2013-10-03 Magnus Feuer
%%%-------------------------------------------------------------------

-ifndef(__INPEVT_HRL__).
-define(__INPEVT_HRL__, true).

%% Records used by the input event system: NEEDS WORK

-record(input_event,
        {
	 id :: port()|reference(), %% Port/Reference for the input event
	 sec  :: integer(), %% Seconds since epoch for event
	 usec :: integer(), %% usec siunce secont
	 type :: syn | abs | rel | absinfo | key | removed, %% Type of event 
	 code_sym, %% symbol code FIXME! TIE INTO OTHER RECORDS
	 code_num, %% Numeric symbol
	 value     %% Value of symbol
        }).

-record(syn, { element:: report | config | mt_report | mt_dropped }).


-record(key, { key, code::integer() }).


-record(rel, { element:: x | y | z | rx | ry | rz | hwheel | dial | wheel | misc }).


-record(absinfo, { value::integer(), minimum::integer(), maximum::integer(),
                   fuzz::integer(), flat::integer(), resolution::integer() }).


-record(abs, { element:: x | y | z | rx | ry | rz | throttle | rudder | wheel | gas |
                         brake | hat0x | hat0y | hat1x | hat1y | hat2x | hat2y | hat3x |
                         hat3y | pressure | distance | tilt_x | tilt_y | tool_width | volume | misc |
                         mt_slot | mt_touch_major | mt_touch_minor | mt_width_major | mt_width_minor |
                         mt_orientation | mt_position_x | mt_position_y | mt_tool_type | mt_blob_id |
                         mt_tracking_id | mt_pressure | mt_distance,  spec::#absinfo{} }).


-record(sw, { element:: lid | tablet_mode | headphone_insert | rfkill_all | radio | microphone_insert |
                        dock | lineout_insert | jack_physical_insert | videoout_insert |
                        camera_lens_cover | keypad_slide | front_proximity | rotate_lock |
                        linein_insert }).

-record(cap_spec, { element::#syn {} | #key {} | #rel {} | #abs {} | #sw {} }).

-record(drv_dev_id, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).

-record(descriptor, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).

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

-endif.
