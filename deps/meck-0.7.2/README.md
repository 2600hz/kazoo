[![Build Status](https://secure.travis-ci.org/eproxus/meck.png)](http://travis-ci.org/eproxus/meck)

  * [Introduction](#introduction)
  * [Features](#features)
  * [Examples](#examples)
  * [Build](#build)
  * [Install](#install)
  * [Contribute](#contribute)

meck
====
A mocking library for Erlang.


<a name='introduction'>

Introduction
------------

With meck you can easily mock modules in Erlang. You can also perform
some basic validations on the mocked modules, such as making sure no
unexpected exceptions occurred or looking at the call history.


<a name='features'>

Features
--------

  * Automatic renaming and restoration of original modules
  * Automatic backup and restore of cover data
  * Changing return values using sequences and loops of static values
  * Pass through: use functions from the original module
  * Mock is linked to the creating process (disable with `no_link`)
  * Complete call history showing calls, results and exceptions
  * Mocking of sticky modules (using the option `unstick`)
  * Throwing of expected exceptions that keeps the module valid


<a name='examples'>

Examples
--------
Here's an example of using meck in the Erlang shell:

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(dog).
ok
2> meck:expect(dog, bark, fun() -> "Woof!" end).
ok
3> dog:bark().
"Woof!"
4> meck:validate(dog).
true
5> meck:unload(dog).
ok
6> dog:bark().
** exception error: undefined function dog:bark/0
```

Exceptions can be anticipated by meck (resulting in validation still
passing). This is intended to be used to test code that can and should
handle certain exceptions indeed does take care of them:

```erl
5> meck:expect(dog, meow, fun() -> meck:exception(error, not_a_cat) end).
ok
6> catch dog:meow().
{'EXIT',{not_a_cat,[{meck,exception,2},
                    {meck,exec,4},
                    {dog,meow,[]},
                    {erl_eval,do_apply,5},
                    {erl_eval,expr,5},
                    {shell,exprs,6},
                    {shell,eval_exprs,6},
                    {shell,eval_loop,3}]}}
7> meck:validate(dog).
true
```

Normal Erlang exceptions result in a failed validation. The following
example is just to demonstrate the behavior, in real test code the
exception would normally come from the code under test (which should,
if not expected, invalidate the mocked module):

```erl
8> meck:expect(dog, jump, fun(Height) when Height > 3 ->
                                  erlang:error(too_high);
                             (Height) ->
                                  ok
                          end).
ok
9> dog:jump(2).
ok
10> catch dog:jump(5).
{'EXIT',{too_high,[{meck,exec,4},
                   {dog,jump,[5]},
                   {erl_eval,do_apply,5},
                   {erl_eval,expr,5},
                   {shell,exprs,6},
                   {shell,eval_exprs,6},
                   {shell,eval_loop,3}]}}
11> meck:validate(dog).
false
```

Here's an example of using meck inside an EUnit test case:

```erlang
my_test() ->
    meck:new(my_library_module),
    meck:expect(my_library_module, fib, fun(8) -> 21 end),
    ?assertEqual(21, code_under_test:run(fib, 8)), % Uses my_library_module
    ?assert(meck:validate(my_library_module)),
    meck:unload(my_library_module).
```

Pass-through is used when the original functionality of a module
should be kept. When the option `passthrough` is used when calling
`new/2` all functions in the original module will be kept in the
mock. These can later be overridden by calling `expect/3` or
`expect/4`.

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick, passthrough]).
ok
2> string:strip("  test  ").
"test"
```

It's also possible to pass calls to the original function allowing us
to override only a certain behavior of a function (this usage is
compatible with the `passthrough` option). `passthrough/1` will always
call the original function with the same name as the expect is 
defined in):

```erl
Eshell V5.8.4  (abort with ^G)
1> meck:new(string, [unstick]).
ok
2> meck:expect(string, strip, fun(String) -> meck:passthrough([String]) end).
ok
3> string:strip("  test  ").
"test"
4> meck:unload(string).
ok
5> string:strip("  test  ").
"test"
```

<a name='build'>

Build
-----

meck requires [rebar][1] to build. To build meck, go to the meck
directory and simply type:

```sh
rebar compile
```

To make sure meck works on your platform, run the tests:

```sh
rebar eunit
```

Two things might seem alarming when running the tests:

  1. Warnings emitted by cover
  2. En exception printed by SASL

Both are expected due to the way Erlang currently prints errors. The
important line you should look for is `All XX tests passed`, if that
appears all is correct.


<a name='install'>

Install
-------

To install meck permanently, use of [Agner][2] is recommended:

```sh
agner install meck
```

If you want to install your own built version of meck add the ebin
directory to your Erlang code path or move the meck folder into your
release folder and make sure that folder is in your `ERL_LIBS`
environment variable.


<a name='contribute'>

Contribute
----------

Patches are greatly appreciated!

Should you find yourself using meck and have issues, comments or
feedback please [create an issue here on GitHub.] [3]

  [1]: https://github.com/basho/rebar "Rebar - A build tool for Erlang"
  [2]: http://erlagner.org/ "Agner - Erlang Package Index & Package Manager"
  [3]: http://github.com/eproxus/meck/issues "meck issues"
