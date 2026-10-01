# PF5 Project

## Installing `opam`

To get started, install the [`opam`](https://opam.ocaml.org/) package manager by following the instructions provided [here](https://opam.ocaml.org/doc/Install.html).

## Installing Packages

Navigate to the cloned project directory.
From there, run the following commands to create a local opam switch and install the required packages:

```OCaml
opam update
opam switch create . 4.14.1 -y --deps-only
```

## Compilation

To compile the project, run: `make`.

## Toplevel

For testing and debugging, you can use the `utop` toplevel, which has been installed with the project dependencies.
To launch it, run: `make top`.

## Tests

To run all available tests, run: `make test`.
To test only the functions from exercise *i*, run: `make test-i`.

## Running the Interpreter

The file containing the project's main function is `bin/interp.ml` .
The command to build the project is: `dune build`,
To build and run the main program, use: `dune exec interp` followed by any desired options and arguments.
Example: 
```OCaml
dune exec interp -- -abs 10 10 20 20 -cr -bc 255 255 255 -fc 0 0 0 -rc 255 0 0 -pc 0 0 255 -size 1080 720 -print 1
```

## Options

-abs X_MIN Y_MIN X_MAX Y_MAX :
    Defines the display area for rectangles and the initial approximation, which must contain the point (0, 0).

-cr :
    Displays points in the simulation.

-bc r v b :
    Sets the background color of the window. The r, v, and b values represent the red, green, and blue components, respectively (from 0 to 255).

-fc r v b :
    Sets the foreground color (the main element of the program, such as rectangles or points). The r, v, and b values represent the red, green, and blue components, respectively (from 0 to 255).
-rc r v b :
    Sets the rectangle color.

-pc r v b :
    Sets the point color.

-size W H :
    Sets the window size in pixels. W is the width and H is the height.

-start X Y :
    Sets the starting point of the program. By default, it is initialized to (0,0).

-print (WIP):
    Displays the lines of code being executed in the standard output.

## Final Remarks

- We did not have enough time to finish implementing the -print option.
- The r key, which was previously used to force resizing, is no longer really necessary now that the loop is dynamic.
