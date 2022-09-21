#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=inpevt
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = dterm.o inpevt_driver.o
LIBS = 

all: ../priv/$(APP)_driver.so ../priv/uinput_test

../priv/$(APP)_driver.so: $(OBJS)
	$(CC) -o$@ $(LDFLAGS) $(OBJS) $(LIBS) 

../priv/uinput_test: uinput_test.o
	$(CC) -o$@ uinput_test.o 

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
