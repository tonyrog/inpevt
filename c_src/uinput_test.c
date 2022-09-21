//
// test program for input - sending virtual key events
//
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <memory.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <linux/uinput.h>

int emit(int fd, int type, int code, int val)
{
   struct input_event ie;

   ie.type = type;
   ie.code = code;
   ie.value = val;
   /* timestamp values below are ignored */
   ie.time.tv_sec = 0;
   ie.time.tv_usec = 0;

   return write(fd, &ie, sizeof(ie));
}

void setup_keyboard(int fd)
{
    struct uinput_setup usetup;
   /*
    * The ioctls below will enable the device that is about to be
    * created, to pass key events, in this case the space key.
    */
   ioctl(fd, UI_SET_EVBIT, EV_KEY);
   ioctl(fd, UI_SET_KEYBIT, KEY_SPACE);
   ioctl(fd, UI_SET_KEYBIT, KEY_ENTER);
   ioctl(fd, UI_SET_KEYBIT, KEY_ESC);    
   ioctl(fd, UI_SET_KEYBIT, KEY_A);
   ioctl(fd, UI_SET_KEYBIT, KEY_B);
   ioctl(fd, UI_SET_KEYBIT, KEY_C);

   memset(&usetup, 0, sizeof(usetup));
   usetup.id.bustype = BUS_USB;
   usetup.id.vendor  = 0xFEED; /* sample vendor */
   usetup.id.product = 0x1111; /* sample product */
   strcpy(usetup.name, "Test Keyboard");

   ioctl(fd, UI_DEV_SETUP, &usetup);
   ioctl(fd, UI_DEV_CREATE);
}

void send_keyboard_events(int fd, char* keys)
{
    int c;
    while((c = *keys++)) {
	int key;
	switch(c) {
	case 'a':   key = KEY_A; break;
	case 'b':   key = KEY_B; break;
	case 'c':   key = KEY_C; break;
	case '\n':  key = KEY_ENTER; break;
	case ' ':   key = KEY_SPACE; break;
	default:    key = KEY_ESC; break;
	}
	emit(fd, EV_KEY, key, 1);  // press
	emit(fd, EV_SYN, SYN_REPORT, 0);
	emit(fd, EV_KEY, key, 0);  // release
	emit(fd, EV_SYN, SYN_REPORT, 0);
    }
}

void setup_mouse(int fd)
{
    struct uinput_setup usetup;

    /* enable mouse button left and relative events */
    ioctl(fd, UI_SET_EVBIT, EV_KEY);
    ioctl(fd, UI_SET_KEYBIT, BTN_LEFT);

    ioctl(fd, UI_SET_EVBIT, EV_REL);
    ioctl(fd, UI_SET_RELBIT, REL_X);
    ioctl(fd, UI_SET_RELBIT, REL_Y);

    memset(&usetup, 0, sizeof(usetup));
    usetup.id.bustype = BUS_USB;
    usetup.id.vendor  = 0xFEED; /* sample vendor */
    usetup.id.product = 0x2222; /* sample product */
    strcpy(usetup.name, "Test Mouse");

    ioctl(fd, UI_DEV_SETUP, &usetup);
    ioctl(fd, UI_DEV_CREATE);
}

void send_mouse_events(int fd, int* x, int *y, int n)
{
    while (n--) {
	emit(fd, EV_REL, REL_X, *x++);
	emit(fd, EV_REL, REL_Y, *y++);
	emit(fd, EV_SYN, SYN_REPORT, 0);
	usleep(15000);
    }
}

int main(int argc, char** argv)
{
    int fd;

    if ((fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK)) < 0) {
	fprintf(stderr, "unable to open /dev/uinput: %s\n", strerror(errno));
	exit(1);
    }

    if (strcmp(argv[1], "keyboard") == 0) {
	setup_keyboard(fd);
	sleep(2);  // wait for udev/application etc ...
	send_keyboard_events(fd, "a b c\n");
    }
    else if (strcmp(argv[1], "mouse") == 0) {
	int vx[] = {5,5,5,5,10,10,10,10,10,-5,-5,-5,-5,-5,-10,-10,-10,-10};
	int vy[] = {-5,-5,-5,-5,-10,-10,-10,-10,-10,5,5,5,5,5,10,10,10,10};
	setup_mouse(fd);
	sleep(2);  // wait for udev ...
	send_mouse_events(fd, vx, vy, sizeof(vx)/sizeof(vx[0]));
    }
    else {
	fprintf(stderr, "usage: uinput_test [mouse|keyboard]\n");
	exit(1);
    }

    /*
     * Give userspace some time to read the events before we destroy the
     * device with UI_DEV_DESTOY.
     */
    sleep(1);
    
    ioctl(fd, UI_DEV_DESTROY);
    close(fd);
    
    return 0;
}

