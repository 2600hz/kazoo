Mustache for Erlang [![Build Status](https://secure.travis-ci.org/mojombo/mustache.erl.png?branch=master)](http://travis-ci.org/mojombo/mustache.erl)
===================

An Erlang port of [Mustache for Ruby][1]. Mustache is a framework-agnostic
templating system that enforces separation of view logic from the template
file. Indeed, it is not even possible to embed logic in the template. This
allows templates to be reused across language boundaries and for other
language independent uses.

This project uses [Semantic Versioning](http://semver.org) for release
numbering.

Working with Mustache means dealing with templates, views, and contexts.
Templates contain HTML (or some other format) and Mustache tags that specify
what data to pull in. A template can be either a string or a file (usually
ending in .mustache). Views are Erlang modules that can define functions that
are called and provide the data for the template tags. A context is an Erlang
dict that contains the current context from which tags can pull data. A few
examples will clarify how these items interact.

NOTE: This is alpha software. Do not use it in production without extensive
testing. The API may change at any time. It still lacks some of the features
of Mustache for Ruby and the performance (even with compiled templates) is not
yet where I'd like it to be.


Installation
------------

To compile the code, navigate to the Mustache.erl project root and issue:

    make

This will produce a `mustache.beam` file in the `ebin` directory that must be
included in the code path of projects that need it.


The Simplest Example
--------------------

The simplest example involves using a string template and a context from the
REPL.

    1> Ctx = dict:from_list([{planet, "World!"}]).
    {dict,1,16,16,8,80,48,...}

    2> mustache:render("Hello {{planet}}", Ctx).
    "Hello World!"

In line 1 we created a context that contains a value bound to the `planet`
tag. In line 2 we render a string template by passing in the template and the
context.


Two-File Example
----------------

A more complex example consists of two files: the view and the template. The
view (logic) file is an Erlang module (simple.erl):

    -module(simple).
    -compile(export_all).

    name() ->
      "Tom".

    value() ->
      10000.

    taxed_value() ->
      value() - (value() * 0.4).

    in_ca() ->
      true.

In the view we define functions that will be called by the template. The names
of the functions correspond to the tag names that will be used in the
template. Some functions reference others, some return values, and some return
only booleans.

The template file (simple.mustache) looks like so:

    Hello {{name}}
    You have just won ${{value}}!
    {{#in_ca}}
    Well, ${{ taxed_value }}, after taxes.
    {{/in_ca}}

Notice that the template references the functions in the view module. The
return values from the view dictate how the template will be rendered. To get
the HTML output, make sure the `simple.beam` bytecode file is in your code
path and call the following function:

    mustache:render(simple)

This tells Mustache to use the `simple` view and to look for a template named
`simple.mustache` in the same directory as the `simple.beam` bytecode file. If
all goes well, it returns the rendered HTML:

    Hello Tom
    You have just won $10000!
    Well, $6000.00, after taxes.


Compiled Templates (for speed)
------------------------------

In order to boost performance for templates that will be called many times in
the lifetime of a runtime, Mustache allows you to compile a template and then
provide that to the render function (instead of having to implicitly recompile
the template on each call).

    1> TFun = mustache:compile(simple).
    2> mustache:render(simple, TFun).

Now, each call to render will use the compiled template (TFun) instead of
compiling the template on its own.


The Power of Context
--------------------

You will often want to provide additional data to your template and view. You
can do this by passing in an initial context to the render function. During
rendering, tag lookups always hit the context first before looking for a view
function. In this way, the context can be used to override view functions.
Using the same template and view as above, we can replace the name tag with
different data by constructing a context and passing it to `render`:


    1> Ctx = dict:from_list([{name, "Chris"}]).
    1> TFun = mustache:compile(simple).
    2> mustache:render(simple, TFun, Ctx).

This will produce the following output:

    Hello Chris
    You have just won $10000!
    Well, $6000.00, after taxes.

The context is also accessible from view functions, making it easy to pass in
initialization data. Consider a case where we want to pass in a user ID:

    Ctx = dict:from_list([{id, 42}])

View functions can get access to the context by accepting a single argument:

    name(Ctx) ->
      ...

Now when this function is called, it will be handed the context. In order to
fetch data from the context, use `mustache:get/2`:

    name(Ctx) ->
      Id = mustache:get(id, Ctx),
      ...

If the requested key does not exist in the context, the empty list `[]` will
be returned.


Tag Types
---------

Tags are indicated by the double mustaches. `{{name}}` is a tag. Let's talk
about the different types of tags.

### Variables

The most basic tag is the variable. A `{{name}}` tag in a basic template will
try to call the `name` function on your view. By default a variable "miss"
returns an empty string.

All variables are HTML escaped by default. If you want to return unescaped
HTML, use the triple mustache: `{{{name}}}`.

### Boolean Sections

A section begins with a pound and ends with a slash. That is,
`{{#person}}` begins a "person" section while `{{/person}}` ends it.

If the `person` method exists and calling it returns `false`, the HTML
between the pound and slash will not be displayed.

If the `person` method exists and calling it returns `true`, the HTML
between the pound and slash will be rendered and displayed.

### List Sections

List sections are syntactically identical to boolean sections in that they
begin with a pound and end with a slash. The difference, however, is in the
view: if the function called returns a list, the section is repeated as the
list is iterated over.

Each item in the enumerable is expected to be a dict that will then become the
context of the corresponding iteration. In this way we can construct loops.

For example, imagine this template:

    {{#repo}}
      <b>{{name}}</b>
    {{/repo}}

And this view code:

    repo() ->
      [dict:from_list([{name, Name}]) || Name <- ["Tom", "Chris", "PJ"]].

When rendered, our view will contain a list of each of the names in the source
list.

### Comments

Comments begin with a bang and are ignored. The following template:

    <h1>Today{{! ignore me }}.</h1>

Will render as follows:

    <h1>Today.</h1>


TODO
----

* Support partials
* Learn some things from erlydtl (speed improvments, perhaps)


Meta
----

* Code: `git clone git://github.com/mojombo/mustache.erl.git`

[1]: http://github.com/defunkt/mustache.git