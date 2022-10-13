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

// extern int usleep(useconds_t usec);


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
   ioctl(fd, UI_SET_KEYBIT, KEY_LEFTCTRL);
   ioctl(fd, UI_SET_KEYBIT, KEY_RIGHTCTRL);   
   ioctl(fd, UI_SET_KEYBIT, KEY_LEFTSHIFT);
   ioctl(fd, UI_SET_KEYBIT, KEY_RIGHTSHIFT);   
   ioctl(fd, UI_SET_KEYBIT, KEY_C); // copy
   ioctl(fd, UI_SET_KEYBIT, KEY_X); // cut
   ioctl(fd, UI_SET_KEYBIT, KEY_V); // paste

   ioctl(fd, UI_SET_KEYBIT, KEY_CUT);
   ioctl(fd, UI_SET_KEYBIT, KEY_COPY);
   ioctl(fd, UI_SET_KEYBIT, KEY_PASTE);
   ioctl(fd, UI_SET_KEYBIT, KEY_PLAYCD);

   memset(&usetup, 0, sizeof(usetup));
   usetup.id.bustype = BUS_USB;
   usetup.id.vendor  = 0xFEED; /* sample vendor */
   usetup.id.product = 0x1111; /* sample product */
   strcpy(usetup.name, "UInput Keyboard");

   ioctl(fd, UI_DEV_SETUP, &usetup);
   ioctl(fd, UI_DEV_CREATE);
}

void send_key_press(int fd, int key)
{
    emit(fd, EV_KEY, key, 1);
    emit(fd, EV_SYN, SYN_REPORT, 0);    
}
 
void send_key_release(int fd, int key)
{
    emit(fd, EV_KEY, key, 0);
    emit(fd, EV_SYN, SYN_REPORT, 0);    
}

void send_key(int fd, int ctrl, int shift, int key)
{
    if (ctrl) {
	emit(fd, EV_KEY, KEY_LEFTCTRL, 1);
    }
    if (shift) {
	emit(fd, EV_KEY, KEY_RIGHTSHIFT, 1);
    }
    emit(fd, EV_KEY, key, 1);
    emit(fd, EV_SYN, SYN_REPORT, 0);
    
    emit(fd, EV_KEY, key, 0);
    if (shift) {
	emit(fd, EV_KEY, KEY_RIGHTSHIFT, 0);
    }
    if (ctrl) {
	emit(fd, EV_KEY, KEY_LEFTCTRL, 0);
    }
    emit(fd, EV_SYN, SYN_REPORT, 0);
}

void send_keyboard_events(int fd, char* keys)
{
    int c;
    while((c = *keys++)) {
	switch(c) {
	case 'a': send_key(fd, 0, 0, KEY_A); break;
	case 'A': send_key(fd, 0, 1, KEY_A); break;
	case 'b': send_key(fd, 0, 0, KEY_B); break;
	case 'B': send_key(fd, 0, 1, KEY_B); break;
	case 'c': send_key(fd, 0, 0, KEY_C); break;	    
	case 'C': send_key(fd, 0, 1, KEY_C); break;
	case 'x': send_key(fd, 0, 0, KEY_X); break;
	case 'X': send_key(fd, 0, 1, KEY_X); break;	    
	case 'v': send_key(fd, 0, 0, KEY_V); break;
	case 'V': send_key(fd, 0, 1, KEY_V); break;	    	    
	case '\n': send_key(fd, 0, 0, KEY_ENTER); break;
	case ' ': send_key(fd, 0, 0, KEY_SPACE); break;
	default:  send_key(fd, 0, 0, KEY_ESC); break;
	}
    }
}

void send_keyboard_cut(int fd, int style)
{
    switch(style) {
    case 0: send_key(fd, 1, 0, KEY_X); break;
    case 1: send_key(fd, 1, 1, KEY_X); break;
    case 2: send_key(fd, 0, 0, KEY_CUT); break;
    }
}

void send_keyboard_copy(int fd, int style)
{
    switch(style) {
    case 0: send_key(fd, 1, 0, KEY_C); break;
    case 1: send_key(fd, 1, 1, KEY_C); break;
    case 2: send_key(fd, 0, 0, KEY_COPY); break;
    }
}

void send_keyboard_paste(int fd, int style)
{
    switch(style) {
    case 0: send_key(fd, 1, 0, KEY_V); break;
    case 1: send_key(fd, 1, 1, KEY_V); break;
    case 2: send_key(fd, 0, 0, KEY_PASTE); break;
    }    
}

void send_playcd(int fd)
{
    send_key(fd, 0, 0, KEY_PLAYCD);
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
    strcpy(usetup.name, "UInput Mouse");

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

    printf("sizeof(struct input_event) = %ld\n",
	   sizeof(struct input_event));

    if ((fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK)) < 0) {
	fprintf(stderr, "unable to open /dev/uinput: %s\n", strerror(errno));
	exit(1);
    }

    if (strcmp(argv[1], "keyboard") == 0) {
	setup_keyboard(fd);
	sleep(2);  // wait for udev/application etc ...
	send_keyboard_events(fd, "a B c");
    }
    else if (strcmp(argv[1], "cut") == 0) {
	setup_keyboard(fd);
	sleep(2);  // wait for udev/application etc ...
	send_keyboard_cut(fd, 1);
    }
    else if (strcmp(argv[1], "copy") == 0) {
	setup_keyboard(fd);
	sleep(2);  // wait for udev/application etc ...
	send_keyboard_copy(fd, 1);
    }
    else if (strcmp(argv[1], "paste") == 0) {
	setup_keyboard(fd);
	sleep(2);  // wait for udev/application etc ...
	send_keyboard_paste(fd, 1);
    }
    else if (strcmp(argv[1], "playcd") == 0) {
	setup_keyboard(fd);
	printf("delay 5 seconds...\n");
	sleep(5);  // wait for udev/application etc ...
	send_playcd(fd);
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

