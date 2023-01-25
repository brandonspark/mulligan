# mulligan

`mulligan` is an interpreter-style debugger for the Standard ML language, aiming to implement
the semantics of SML at a fine granularity for inspection purposes.

Has rudimentary support for SML/NJ CM files, allowing analysis of entire SML/NJ
projects.

<p align="center">
<img src="https://user-images.githubusercontent.com/49291449/214633031-1f33f23d-af7f-4116-ba9d-fb8cb8f2a9e4.svg" width="700">
</p>

## Usage

To use `mulligan`, you must first build it by running `make`. Then, you can pass
in the desired file for evaluation via the commandline.
```
$ make
$ ./mulligan test.sml
```

This will take you into the `mulligan` interactive loop.

`mulligan` may accept both `.sml`, `.sig`, `.fun` files, and `.cm` files, upon
which it will traverse the sources, exporting the correct bindings on conclusion.
`mulligan` does not currently support computing a dependency graph for CM dependencies,
and thus always evaluates the source files _in order of listing_.

## Features

Upon evaluating a particular program, `mulligan` takes the user into an
interactive loop, in which the user can take a variety of actions to manipulate
the program state.

![image](https://user-images.githubusercontent.com/49291449/173886267-d956cfea-6aa0-4db8-ad3d-5b8fde1905ec.png)

The commands `step` and `prev` allow the user to step through execution, until
the next "significant event". Significant events include stepping of a
sub-expression, function application, and binding to an identifier, among
others. Some of these may be adjusted in the loop.

By default, while stepping `mulligan` will display the surrounding context until the
nearest declaration site. This can make it confusing which expression is currently being focused,
so the `reveal <i>` command allows one to see the surrounding context for the currently
evaluated expresison, up to `<i>` layers deep.

`mulligan` also allows you to print the value bound to a particular identifier,
via the command `print <id>`.

There are some internal settings which control the pretty-printer, primarily,
which can be changed via `set <name> = <v>`. These are explained in more detail
via `mulligan --help`.

## Breakpoints

`mulligan` allows one to set _breakpoints_, which are particular events that
should be skipped to, instead of needing to be stepped to manually.

The command `break <id>` sets a breakpoint on the function value bound to the
identifier `<id>`, which will be triggered upon entering that function's body.
It is worth noting that this is truly a breakpoint on that function _value_, as
even if it is bound to another identifier and invoked, execution will be
stopped.

The command `break bind <id>` sets a breakpoint on binding values to the identifier
`<id>`. This will be triggered upon encountering a val declaration to that
identifier.

The command `run` allows one to skip to the end of program execution, or to the
first breakpoint. The command `last` allows one to skip to the beginning of
program execution, or to the last breakpoint that was tripped.

The command `clear` clears all breakpoints.

# Naming

Naming has followed the same spirit as another SML project, [millet](https://github.com/azdavis/millet).

`mulligan` has the letters "m" and "l" in it, in that order. So does "Standard
ML".

Also, this debugger allows you to rewind and redo evaluation of a program, and a
mulligan is a term for redoing an action in a game.

# License

The included LICENSE file is the license for this project.
