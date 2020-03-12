# Unit- and Property-based testing in Kazoo

## Unit Testing

Kazoo uses EUnit for the majority of unit testing. Running `make eunit` from the root or within an Erlang application directory will run the tests of the project or app respectively. Modules will be cover-compiled as well, generating a cover report that can be viewed in a browser.

!!! note
    If you are using Emacs (and you should be) you can serve the cover directory by making sure you have the `simple-httpd` package installed, then run `httpd-start` followed by `httpd-serve-directory` to chose the cover directory of choice.

## Property Testing

Kazoo uses PropEr for doing property-based testing.

## Function call tracing

It can be helpful, when testing, to trace the function calls made (and their arguments). The `dbg` module provides nice trace functionality but can be a bit arcane to get working.

In `kazoo_stdlib/include/kazoo_dbg.hrl` there are now macros to make it easy to trace calls in modules, `module:function`, or `module:function/arity`.

For instance:

```erlang
-include_lib("kazoo_stdlib/include/kazoo_dbg.hrl").
%% To trace all function calls made in the 'my_module' module:
some_test() ->
    ?DBG_START,
    ?DBG_TRACE('my_module'),
    Result = my_module:do_stuff(),
    ?DBG_STOP,
    ?assert(Result).
```

Do note that the `dbg` module has many other ways to trace; the macros currently just cover a basic, often-used one.

Do note as well that these should not be used in production code.
