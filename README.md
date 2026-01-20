**PLUS 4 EMU**

This is a simple Commodore Plus 4 emulator for the Mega65 computer

The latest build is in the Target directory as a d81

It uses screen mirroring rather than scanline rendering to make it faster than it would be otherwise.

Current Features:
* Standard LOAD and SAVE work, but not extended DOS commands like DIRECTORY.  LOAD/SAVE were basic enough to hook into and load the disk directory (LOAD"$",8), or load/save your own programs.
* Bitmap and multicolor GRAPHIC mode commands work
* Provides a full 64kb machine.  Not a gimped C16

To Do:
* Cursor keys do not yet work.  It's annoying but Ill get to it.
* Other DOS commands.  OPEN/CLOSE/COLLECT, etc.. not just yet
* The TED has 121(?) colors due to luminence. The emulation currently does not support luminence, so colors will be off from a real machine.  It tries.  Give it a break. :)
* Always looking to optimize.  Its slow and probably will never be cycle / timing exact
* Sound
* If it ever is capable of graphics games, Ill add joystick support
* Fix bugs

So, for now, its a foundation.  Have fun!  Pull requests are welcome.
