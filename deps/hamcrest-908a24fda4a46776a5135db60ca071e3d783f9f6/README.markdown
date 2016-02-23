Hamcrest Erlang [![travis](https://secure.travis-ci.org/hyperthunk/hamcrest-erlang.png)](http://travis-ci.org/hyperthunk/hamcrest-erlang)
=============================

This is an implementation of Hamcrest for the
[Erlang programming language](http://www.erlang.org/). This tutorial has largely
been cribbed from the hamcrest-python version, upon which the API is also based.

Hamcrest is a framework for writing matcher objects allowing 'match' rules to be
defined declaratively. There are a number of situations where matchers are
invaluable, such as UI validation, or data filtering, but it is in the area of
writing flexible tests that matchers are most commonly used.

This tutorial shows you how to use Hamcrest for unit testing. When writing tests
it is sometimes difficult to get the balance right between over specifying the
test (and making it brittle to changes), and not specifying enough (making the
test less valuable since it continues to pass even when the thing being tested is
broken). Having a tool that allows you to pick out precisely the aspect under
test and describe the values it should have, to a controlled level of precision,
helps greatly in writing tests that are "just right". Such tests fail when the
behaviour of the aspect under test deviates from the expected behaviour, yet continue
to pass when minor, unrelated changes to the behaviour are made.

Building Hamcrest-Erlang
------------------------

Hamcrest-Erlang is built using rebar. You should use the bundled version of the 
rebar executable however, because the build relies on some currently unsupported
and experimental features. The following command will compile the sources and 
generate the main header file:

    $ ./rebar clean && ./rebar compile

Please note that currently there is a bug in the build plugins that
requires these steps to be executed separately.

My first Hamcrest test
------------------------

We'll start be writing a very simple EUnit test, but instead of
using EUnit's ?assertEqual macro, we use Hamcrest's `assert_that`
construct and the standard set of matchers:

```erlang
-module(demo1_tests).
-compile(export_all).

-import(hamcrest, [assert_that/2]).

-include_lib("hamcrest/include/hamcrest.hrl").

using_assert_that_test() ->
    assert_that(10, is(greater_than(2))).  %% returns true
```

The assert_that function is a stylised sentence for making a test
assertion. In this example, the subject of the assertion is the number
10 that is the first method parameter. The second method parameter is a
matcher for numbers, here a matcher that checks one number is greater
than another using the standard > operator. If the test passes (as it
will in this case) then assert_that returns the atom 'true' by default.

Standard Matchers
------------------

The standard matchers supplied with hamcrest (Erlang) are to be found in the hamcrest_matchers module. They are:

* match_mfa/3:  takes an MFA triplet (module, function, initial args), produces a spec that matches if `apply(Mod, Func, Args ++ [Input])` evaluates true
* will_fail/0:  takes a zero arity fun and matches if it "fails" when executed
* will_fail/2:  takes a zero arity fun and matches if it "fails" with the supplied class (error,exit,exception) and reason (term)
* isalive/0:    takes Pid and matches if `is_pid(Pid) andalso erlang:is_process_alive(Pid)` evaluates true
* isdead/0:     takes Pid and matches if `is_pid(Pid) andalso erlang:is_process_alive(Pid)` evaluates false
* anything/0:   always matches - useful in writing mock expectations (with other frameworks) to assert a call was made but ignore its inputs.
* any_of/1:     takes a list of matchers and evaluates true if at least one of them matches the supplied input
* equal_to/1:   matches if X == Y is true
* exactly_equal_to/1:   matches if X =:= Y is true
* has_length/1: takes a number and produces a spec that matches a collection (list, set or gb_set) if the collection is of the specified length.
* contains_member/1: takes a term and produces a spec that matches a collection (list, set or gb_set) if the collection contains the specified element.
* isempty/0:    takes a list, tuple, set or gb_set and matches if it is empty
* is/1:         when passed a value (term), acts exactly like equal_to; when passed a matcher, defers to it providing syntactic sugar
* is_not/1:     inverse of is/1
* greater_than/1: matches if X > Y is true
* greater_than_or_equal_to/1 : matches if X >= Y is true
* less_than/1:  matches if X < Y is true
* less_than_or_equal_to/1: matches if X <= Y is true
* contains_string/1: matches if string Y contains string X
* starts_with/1: matches if string Y starts with string X
* ends_with/1:   matches if string Y ends with string X

The match_mfa/3 function is a building block for creating custom matchers without having to dig around in the API too deeply.

Using hamcrest assertions
----------------------------

The `assert_that/2` and `assert_that/3` functions are the workhorses of
the library. Each function takes an *actual* value and a match specification
(more on this later) and apply the match function to the *actual* input value.
If the match function evaluates to `false`, the descriptor on the match
specification is used to construct and error message and an exception is thrown.

The two and three argument `?assertThat` macros work in exactly the same way,
except they borrow their error reporting mechanism from eunit and provide
information about the line number and expectation failure.

Writing custom matchers
----------------------------

Hamcrest comes bundled with a few useful matchers, but you'll
probably find that you need to create your own from time to time.
This commonly occurs when you find a fragment of code that tests
the same set of properties over and over again (and in different tests),
and you want to bundle the fragment into a single assertion.

Let's write our own matcher for testing if a string is comprised
of only digits. This is the test we want to write:

```erlang
string_is_only_digits_test() ->
    assert_that("12345", is(only_digits())).
```

And here's the implementation:

```erlang
-module(custom_matchers).

-include("../include/hamcrest.hrl").

-export([only_digits/0]).

only_digits() ->
    fun only_digits/1.

only_digits(X) ->
    case re:run(X, "^[\\d]*$") of
        {match,_} -> true;
        _ -> false
    end.
```

The zero arity factory function we exported is responsible
for creating the matcher fun, which should take 1 argument
and return the atom 'true' if it succeeds, otherwise 'false'.
The matcher fun must however be wrapped in a record, which also
allows you to specify the expected versus actual input, and a textual
description that will be evaluated by hamcrest:assert_that/2 in
case of match failures. The `?MATCHER` macro simply takes the
match function, description and expected input domain and generates
an instance of this record.

The desc field can contain a format string to be evaluated against
the expected and actual values, or a function that returns a textual
description instead. In the former case, the format string will be
evaluated as `lists:flatten(io_lib:format(Desc, [Expected, Actual]))`
and the hamcrest:message/4 function exists to provide a simple wrapper
around that feature, allowing you to specify the whether the input is
a string, an error tuple or another (arbitrary) term. It evaluates its
arguments to produce a string that looks something like
`expected a <TYPE> <DESC> <EXPECTED>, but was <ACTUAL>` when called.

Re-using custom matchers
------------------------

Whilst `only_digits/1` is a useful little function, we can improve on
this by generalising it to handle any supplied regex compatible with
the 're' module. Doing this makes the function re-usable in other contexts,
and in fact there is a matcher for this already in the base library!

The `matches_regex` matcher is defined in `hamcrest_matchers` as

```erlang
matches_regex(Rx) ->
    #'hamcrest.matchspec'{
        matcher     =
                    fun(X) ->
                        case re:run(X, Rx) of
                            {match,_} -> true;
                            _         -> false
                        end
                    end,
        desc        = fun(Expected,Actual) ->
                        message(string, "matching the regex", Expected, Actual)
                      end,
        expected    = Rx
    }.
```

Given a definition such as this, our `is_digits` matcher becomes much simpler,
as would `is_alpha_numeric` and many other string matches that regular
expressions would make easy work of.

```erlang
-module(custom_matchers).

-include_lib("hamcrest/include/hamcrest.hrl").
-export([only_digits/0]).

only_digits() ->
    matches_regex("^[\\d]*$").
```

Notice that this time we created the record explicitly instead of using
the `?MATCHER` macro. Yet another way to construct custom matchers is to
use the match_mfa/3 function, which takes a module, function and its
initial arguments and matches if adding the match input to the argument
list and calling `apply(M,F,A)` evaluates to true. This takes away the headache
of having to write the matchspec yourself, at the expense of a slightly less
specific description provided by assert_that if the match fails. Here's an
example usage taken from the tests and another taken from the core API itself:

```erlang
isalive() ->
  match_mfa(erlang, is_process_alive, []).

some_test(_) ->
    IsMemberOf = match_mfa(lists, member, [[a,b,c]]),
    assert_that(a, IsMember).
```
