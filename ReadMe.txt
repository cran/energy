A small utility to send code from Textpad to Rgui
Copyright (c) 2002-2003, Philippe Grosjean (phgrosjean@sciviews.org)
This is public domain code and it comes with no warranty
Compile this code with Microsoft Visual Basic

Usage:
- Place TpR.exe and  VB6FR.DLL in a directory that is in your path (ex: c:\Windows\System32)
- Create tools in Textpad to submit code to Rgui.
These tools must have the following command line:
1) Source in R -> TpR $UNIXFile
2) Execute in R -> TpR $Clip

Note that RGui must be running, otherwise, TpR.exe just issues a beep and exits without any other action. If RGui is iconized, TpR.exe restore its window first, because RGui does not interpret keystrokes when it is iconized!

The source code for compiling with Visual Basic 6 is also provided (TpR.bas, TpR.vbp, TpR.vbw). If you do not want to compile it yourself, or to change code, you do not need these file on your computer.
