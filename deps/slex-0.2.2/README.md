slex
====

[![Build Status](https://travis-ci.org/erlydtl/slex.png?branch=master)](https://travis-ci.org/erlydtl/slex)

Stateful Lexical Analyzer Compiler.


language
========

A slex file describes a stateful scanner which can be compiled
into an Erlang module for tokenizing text.

A slex file consists of:

  * Attributes describing properties of the scanner.
  * Rules for tokenizing the text.
  * Token transformation definitions.
  * Additional code, not affecting the compilation of the scanner.


slex syntax
===========

The slex syntax is rather basic.

identifiers
-----------

Starts with a letter, followed by any number of alphanumeric or underscore characters.

Example: ``foo123``.

numbers
-------

Positive integers only.

Example: ``123``.

strings
-------

Strings are either quoted, using single or double quote (i.e. ' or "),
or, in case of a "symbols only" string, the quotes can be
omitted. Symbols are those characters that doesn't fit in any other
type.

Backslash is used to escape the next character. A few characters has
special meaning (e.g. r, n, t and s) and for the rest it is simply
included in the string.

Example strings:

  ``"foo bar"`` ``'another string'`` ``!@#$`` ``{{\ }}``

In the last string, note how the the space is escaped so the string
doesn't end there.

comments
--------

Comments start with ``%%`` and run until the end of the line.

Example: ``%% this is a comment.``

Comments prefixed with three or more percent signs are included in the
generated `.erl` source file.

Example: ``%%% comment to be included in the generated source.``

keywords
--------

A few identifiers has special meaning, and is treated accordingly.

These are:

  * ``any``: can be used to indicate "any prefix" or "any state".
  * ``skip``: is used to indicate that a rule doesn't affect the
    scanned tokens.
  * ``until``: used to associate a string that ends a state.
  
There are also a number of symbols used as delimiters, and thus has
special meaning. In other words, these needs to be escaped if used in
symbols strings.

These are:

  * ``+``: can indicate either that a rule appends to a scanned token,
    or that a rules prefix should also match the associated string
    that ends the state.
  * ``-``: used to indicate a state that has no associated ending
    string.
  * ``:``: used as delimiter between the different parts of a rule, or
    as the `end of input` prefix marker.
  * ``,``: just a delimiter.
  * ``.``: marks the end of a definition.


attributes
==========

Attributes are prefixed with a dash ``-``, followed by a identifier
and optional arguments and terminated by a period:

  ``-my_attr foo 123.``

Supported attributes:

  * ``module``
  * ``function``
  * ``init_state``
  * ``include``


rules
=====

Rules make out the main functionality of the scanner, and each rule is
applied to the input text, in order of rule priority.

A rule has five parts: ``prio prefix state [, guard] : body.``

  1. Priority. The rules are matched in order of their priority
     (lowest number first).
  2. Prefix. The prefix that should match the input text.
  3. Current state. The state of the scanner for the rule to apply.
  4. Guard expression. Optional expression to further refine if the
     rule applies.
  5. Rule body, as described below.
  
Example rule: ``10 <? in_text: open_tag, in_tag until ?>.``

Will match on input text of "<?" when the scanner is in state
``in_text``, and save a ``open_tag`` token and continue scanning in
state ``in_tag``. The "until ?>" sets the extra data for the state,
which can be used by another rule to detect when this state should
end.

1. priority
-----------

The priority is to allow injection of new rules passed in to the
compiler with the ``extra_data`` option.

All rules are sorted in ascending priority when building the scanner
function clauses.

2. prefix
---------

The prefix is either the keyword ``any`` or a string that should match
the input text for the rule to apply.

3. current state
----------------

The state of the scanner that the rule applies to. There are a few
tricks in play here to specify the properties of the state, see [states].

In addition to the rules given in [states], if the state is suffixed
with a ``+`` sign, the state extra data must also match the prefix in
order for the rule to apply.

Example, to close the ``in_tag`` state given in an earlier example:

  ``20 ?> in_tag+: close_tag, in_text-.``.

This will match on input text of "?>" when the scanner is in state
``in_tag`` with extra data "?>", and save a ``close_tag`` token and
continue in scanning in state ``in_text`` with no extra data.

Instead of ``in_tag+``, we could have said ``any+``, and it would then
match any state with extra data "?>".

4. guard expression (optional)
------------------------------

The guard expression, if provided, should be a Erlang guard expression
on the form: ``expr <guard code...> end``.

5. body
-------

The body is either a combination of action(s) and state transition
separated by comman ``,``, or an Erlang expression that implements the
rule body on the form: ``expr <rule body code...> end``.

Actions are any number of tokens to save, or the keyword ``skip`` if
no tokens are to be saved when this rule applies.

A action token can be an identifier or a string.  For identifiers, the
saved token is on the form: ``{<identifier>, {Row, Col}, Prefix}``,
while for strings it is on the form: ``{'<string>', {Row, Col}}``.

If the identifier is prefixed by a ``+`` sign and the last saved token
also was a ``<identifier>`` then ``Prefix`` is added to the value of
that token instead of adding a new token.

If the identifier is suffixed by ``-<string>``, then the ``<string>``
will be used as value instead of ``Prefix`` for the token.

Example actions:

  * ``literal``, will save ``{literal, Pos, Prefix}``.
  * ``+string``, if previous token was not a ``string`` token:
    ``{string, Pos, Prefix}``, or if previous token was a ``string``,
    will update it: ``{string, _, Prefix ++ _}`` (``_`` indicates previous value).
  * ``foo 'bar' baz-'moot'``, will save three tokens: ``{baz, Pos,
    "moot"}, {bar, Pos}, {foo, Pos, Prefix}``.

After the actions, an optional state transition can be specified by
providing a state name and optionally the extra data in the form of a
string for the state, using the ``until`` keyword.

Example state transitions:

  * ``plain-``, switch to state ``plain``, with no extra data.
  * ``my_state``, switch to ``my_state`` and keep the extra data from
    the current state.
  * ``custom until 'bail'``, switch to state ``custom`` with extra
    data ``"bail"``.

If a state transition is provided, the actions can be left out
entirely, including the separating comma.


states
======

There's two kinds of state: stateless and stateful. The
stateless state is a state that doesn't carry any extra information
besides its name, while a stateful state carries an extra term,
usually a string indicating the text that will end the state (but can
be anything).

When specifying states in slex, a state name by itself refers to a
stateful state. To refer to a stateless state, add a dash ``-`` suffix
to the state name.


token post processing
=====================

Rules for post processing tokens.


erlang code
===========

Erlang code can be supplied in two distinct contexts, at the module
level, as forms, or in a rule, as expressions.

At the module level, it allows to add arbitrary code to the compiled
module, in the form of additional attributes, functions, include
directives and what not.

In rules, expressions can be used to add custom guards or to provide
the implementation body for the rule.

Code can run over multiple lines when newlines are escaped with a
backslash.

Note: there can only be at most one form/expression on a single line
(e.g. only a single ``end`` per line is supported).

Example:

  ``form -record(foo, {bar, baz}) end``
  ``form foo(Bar) -> {baz, Bar} end``
  ``expr H >= $0 andalso H =< $9 end``


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/erlydtl/slex/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
