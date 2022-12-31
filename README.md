# Assembling
(Copied verbatim from [TTT286](https://github.com/Magicrafter13/TTT286).)  
For this project I use the Microsoft Macro Assembler (MASM) version 6.0b.
I'm sure other versions will work, but here's a link to this version if you want
to try it out yourself.
[MASM 6](https://winworldpc.com/product/macro-assembler/6x)

Your path must be setup correctly for MASM (at least have `<path_to_masm>\BIN`
and `<path_to_masm\BINB` in your PATH).
Compile the program by running `nmake` (a utility provided by MASM) to read the
instructions in `Makefile`.

This program should assemble and run perfectly fine in DOSBox.

# Playing
You can either, Assemble the code yourself, or click on the Releases tab and
download an assembled binary (an exe file). (Make sure to download a graphics
file unless you want to play in text only.)
Just type the name of the program at the DOS prompt, while in the same directory
as the game, ie `C:\TandyChess\> chess` and press enter. Then select which mode
you want to use.

I think the controls are pretty intuitive, especially since there are 2
different ways you can play, but if you aren't sure, just press H to bring up
the help screen. (May not work in all graphics modes, so if nothing happens, run
the game in text mode first to learn the controls.)

## Modes
1. Text Mode - Uses a mode 3, 80x25 text. (though it doesn't use that entire
space)

## Why?
After working on my [Tic-Tac-Toe](https://github.com/Magicrafter13/TandyChess)
game, I desired to do something more advanced. I also have yet to write any
chess implementations in any language.
