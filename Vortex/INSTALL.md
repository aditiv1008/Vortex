First, SDL2 must be installed. The installation link can be found here:
  https://www.libsdl.org/download-2.0.php

Alternate methods of installation for various systems:

Debian-based systems:
```
$ sudo apt-get install libsdl2-dev
```
Homebrew on MacOS:
```
$ brew install sdl2
```
MacPorts on MacOS:
```
$ sudo port install libsdl2
```
Afterwards, to finish installation, some OCaml-exclusive libraries must be downloaded, using the command:
```
$ opam install ANSITerminal ocamlsdl2
```