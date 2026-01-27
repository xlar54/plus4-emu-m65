**PLUS 4 EMU**

This is a simple Commodore Plus 4 emulator for the Mega65 computer

The latest build is in the Target directory as a d81

You MUST have the plus4 KERNAL ROM and BASIC ROM on the same disk.  The files in particular are:
 * basic.318006-01  (rename to BASIC.BIN on the disk)
 * kernal.318004-05 (rename to KERNAL.BIN on the disk)

It uses screen mirroring rather than scanline rendering to make it faster than it would be otherwise.

Current Features:
* ALL DOS commands should work as desribed by std Plus 4 documentation.  But do note that they PASS THROUGH to the host disk!  That means that if you try HEADER (format a disk), you will indeed format the disk in the drive!
* Bitmap and multicolor GRAPHIC mode commands work
* Provides a full 64kb machine.  Not a gimped C16
* Cursor keys work, although they were rather difficult to implement.  Still some oddities with them.
* SOUND has been implemented

To Do:
* The TED has 121(?) colors due to luminence. The emulation currently does not support luminence, so colors will be off from a real machine.  It tries.  Give it a break. :)
* Always looking to optimize.  Its slow and probably will never be cycle / timing exact
* If it ever is capable of graphics games, Ill add joystick support
* Fix bugs

So, for now, its a foundation.  Have fun!  Pull requests are welcome.
