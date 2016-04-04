leadsto
=======

leadsto software.

Latest, April 4, 2016
Checking status of software.
The software does work on a mac os x system with SWI-Prolog 7.3.6 installed.
I will test the procedure on another mac.

* Install SWI-Prolog:
  (Exploring using a mac os x disk image from
   http://www.swi-prolog.org/download/stable
   Stopped: It says
   "WARNING This version does not run property on MacOSX El Captain (10.11). 
    Please download the development version, version 7.3.11 or later instead."

    I had already xquartz installed, you may have to do that.
    Going for swi-prolog development version:
      http://www.swi-prolog.org/download/devel
    There another disk image is available. (Checking xquartz, I have version
        2.7.8, ok , SWI-Prolog requires 2.7.7)
    (Dragged the swi prolog icon onto the applications folder)
    The procedure does not make swipl accessible from command line.
* add export PATH="/Applications/SWI-Prolog.app/Contents/MacOS:$PATH" to
$HOME/.bash_profile

* get the LeadsTo sources from leadsto https://github.com/lourensm/leadsto
* run $ make instpl

* I did a minimum number of tests.
  * Moving the installed files to another directory: The generated executables
    are generated in a subdirectory lt of the leadsto.
    I did $mv lt ~/
    The executables seem to work running from this new directory:
    $ ~/lt/bin/leadsto
    ( A warning on the command line
    Warning: locale not supported by Xlib, locale set to C
    Fontconfig warning: ignoring UTF-8: not a valid region tag directory.
    I am not sure about the consequences)
  * Running example files:
    - Awkward that the source directory is not set correctly.
    ( I seem not to have implemented an option in settings to change the default source path)
    +running ~/lt/examples/test/heartsn.lt produces surprisingly familiar
    output. So far so good.
    + Experimentally tried to printout the trace using the printer icon. On
      my mac the trace got sent to the printer without any additional
      interaction (awkward: would like to be able to change layout or output to
      file, but there is a print to file menu option).
    * Editing lt file. Does work, but I do realize how awkward the editor is.
      DO NOT FORGET TO USE A RIGHT MOUSECLICK when it is unclear what to do.
    * After solving a bug
      $ lt/leadsto test.lt works.
    * ttlchecker test:
      loaded spec/simple.fm, right click on PROPERTY DEFINITION ->test<-
      (awkward...) Formula checked.

  Overall: things seem to work. But the interaction with the GUI is awkward.
      



Earlier

Making leadsto available. Work started sept 20 2015.
First focus on setting up the windows version using swi prolog 6.
First test with swi pl 6.4.1

There are lots of requirements for regenerating the installer.
- cygwin
- nsis

