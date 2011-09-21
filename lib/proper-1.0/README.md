Contact information and license
-------------------------------

PropEr (PROPerty-based testing tool for ERlang) is a QuickCheck-inspired
open-source property-based testing tool for Erlang, developed by Manolis
Papadakis, Eirini Arvaniti and Kostis Sagonas. The base PropEr system was
written mainly by Manolis Papadakis, and the stateful code testing subsystem by
Eirini Arvaniti.

You can reach PropEr's developers in the following ways:

*   on the web: at [the project's home page](http://proper.softlab.ntua.gr) or
    [the project's github page](https://github.com/manopapad/proper)
*   by email: take the project's home page URL, remove the `http://` prefix and
    replace the first dot with a @

We welcome user contributions and feedback (comments, suggestions, feature
requests, bug reports, patches etc.).

Copyright 2010-2011 by Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas.

This program is distributed under the GPL, version 3 or later. Please see the
COPYING file for details.


Introduction
------------

Traditional testing methodologies essentially involve software testers writing a
series of test inputs for their programs, along with their corresponding
expected outputs, then running the program with these inputs and observing
whether it behaves as expected. This method of testing, while simple and easy to
automate, suffers from a few problems, such as:

*   Writing test cases by hand is tedious and time consuming.
*   It is hard to know whether the test suite covers all aspects of the software
    under test.

Property-based testing is a novel approach to software testing, where the tester
needs only specify the generic structure of valid inputs for the program under
test, plus certain properties (regarding the program's behaviour and the input-
output relation) which are expected to hold for every valid input. A property-
based testing tool, when supplied with this information, should randomly
produce progressively more complex valid inputs, then apply those inputs to the
program while monitoring its execution, to ensure that it behaves according to
its specification, as outlined in the supplied properties.

Here are a few examples of simple properties a user may wish to test, expressed
in natural language:

*   The program should accept any character string and convert all lowercase
    letters inside the string to uppercase.
*   The program should accept any list of integers. If the input list is at
    least 4 elements long, the program should return the 4th largest integer in
    the list, else it should throw an exception.

PropEr is such a property-based testing tool, designed to test programs written
in the Erlang programming language. Its focus is on testing the behaviour of
pure functions. On top of that, it is equipped with two library modules that can
be used for testing stateful code. The input domain of functions is specified
through the use of a type system, modeled closely after the type system of the
language itself. Properties are written using Erlang expressions, with the help
of a few predefined macros.

PropEr is also tightly integrated with Erlang's type language:

*   Types expressed in the Erlang type language can be used instead of
    generators written in PropEr's own type system as input data specifications.
*   Generators for ADTs can be constructed automatically using the ADTs' API
    functions.
*   PropEr can test functions automatically, based solely on information
    provided in their specs.


Quickstart guide
----------------

*   Obtain a copy of PropEr's sources.
*   Compile PropEr: run `make` (or `make all`, if you also want to build the
    documentation; in that case, you are going to need the `syntax_tools`
    application and a recent version of `EDoc`).
*   Add PropEr's base directory to your Erlang library path, using one of the
    following methods:
    1.   `ERL_LIBS` environment variable: Add the following line to your shell
         startup file (`~/.bashrc` in the case of the Bash shell):

             export ERL_LIBS=/full/path/to/proper

    2.   Erlang resource file: Add the following line to your `~/.erlang` file:

             code:load_abs("/full/path/to/proper").

*   Add the following include line to all source files that contain properties:

        -include_lib("proper/include/proper.hrl").

*   Compile those source files, preferably with `debug_info` enabled.
*   For each property, run:

        proper:quickcheck(your_module:some_property()).


Where to go from here
---------------------

To get started on using PropEr, see the tutorials and testing tips provided on
[PropEr's home page](http://proper.softlab.ntua.gr). On the same site you can
find a copy of PropEr's API documentation (you can also build this from source
if you prefer, by running `make doc`), as well as links to more resources on
property-based testing.


Common Problems
---------------

### Using PropEr in conjunction with EUnit

The main issue is that both systems define a `?LET` macro. To avoid a potential
clash, simply include PropEr's header file before EUnit's. That way, any
instance of `?LET` will count as a PropEr `?LET`.

### Using PropEr under Erlang/OTP R13B03 or older

PropEr makes heavy use of recursive types, which are unsupported on versions of
the Erlang/OTP distribution prior to R13B04. To compile PropEr on such a system,
add `{d,'NO_TYPES'}` to the `erl_opts` option inside `rebar.config`. This
enables the spec+type-stripping parse transform included in PropEr, which fixes
the problem by stripping all type information from PropEr's source files during
compilation.


Incompatibilities with QuviQ's QuickCheck
-----------------------------------------

We have generally tried to keep PropEr's notation and output format as compatible
as possible with QuviQ's QuickCheck, to allow for the reuse of existing testing
code written for that tool. However, incompatibilities are to be expected, since
the two programs probably bear little resemblance under the hood. Here we
provide a nonexhaustive list of known incompatibilities:

*   `?SUCHTHATMAYBE` behaves differently in PropEr.
*   `proper_gen:pick/1` differs from `eqc_gen:pick/1` in return value format.
*   PropEr handles `size` differently from QuickCheck.
*   `proper:module/2` accepts options in the second argument instead of the
    first
