

# Edown - Markdown generated from Edoc #

Copyright (c) 2010 Erlang Solutions Ltd


__Authors:__ [`ulf.wiger@feuerlabs.com`](mailto:ulf.wiger@feuerlabs.com).


Status:
------
More-or-less readable Markdown can be generated.
A doclet needs to be written that also creates 
a markdown-based index and overview. Currently, the 
edoc_doclet creates an index.html and overview.html,
which do not point to the .md files.

To generate markdown edoc, run:

```

edoc:application(App, [{doclet, edown_doclet} | OtherOpts]).

```

The `edown_xmerl` module is used as an xmerl export module.
It converts xmerl's "simple xml" to Markdown syntax. Note that
GH-flavored Markdown allows HTML markup (at least common tags),
but doesn't expand markdown markup inside HTML markup, so the`edown_xmerl` module has to know the context in which it operates.

** Special edown option: **

Using the option `{top_level_readme, {File, BaseHref}}`, a github-friendly
`README.md` in the top directory can be generated from the `overview.edoc`.
This file is the same as the `doc/README.md` file already generated,
but with relative links corrected (using `BaseHref`) so that they actually
work. This step is needed since Github doesn't support relative paths in
Markdown links.

Example:

`{top_level_readme, {"./README.md", "http://github.com/esl/edown"}}`

The conversion function will fetch the current branch name from git,
and fail if it cannot do so.
Github customizations
=====================
`pre` tags are converted into github "fenced" code blocks, i.e.

```
```...'''
```

. If language-specific syntax highlighting is desired,
this can be achieved by adding a 'lang' attribute, e.g.

```
<pre lang="erlang">
incr(X) ->
  %% This should be formatted with Erlang syntax highlighting
  X + 1.
</pre>
```

which should format like this:

```erlang

incr(X) ->
  %% This should be formatted with Erlang syntax highlighting
  X + 1.

```

Rebar customizations
====================
A set of escripts can be found under
[edown/priv/scripts/](http://github.com/esl/edown/blob/master/priv/scripts/), which
can be used to customize the `rebar` built process. The
[rebar.config.script](http://github.com/esl/edown/blob/master/priv/scripts/rebar.config.script)
file should be copied into your application, next to `rebar.config`.
It will sense if `doc` is a current target, and will then include
`edown` in the `deps`; otherwise, it removes it. This way, you will
not have to pull down `edown` unless you really want to build the
docs. It will also locate edown along your path, in which case
it doesn't need to pull it down again.

The script will also start the `inets` application, so that you
can include URLs as part of a `doc_path` option (see below).

Links to other EDown-generated docs
===================================
There is a way to configure Edoc/Edown to get URLs right even
when linking to other Edown-generated docs on Github.

First, you need to specify paths to the `edoc-info` files for
each repository as part of `edoc_opts` in your rebar.config, e.g.

```
   {doc_path, ["http://raw.github.com/uwiger/setup/master/doc",
               "http://raw.github.com/uwiger/gproc/master/doc"]}
```

Note (1) that we use "http:://...", not "https://...", since
Edoc doesn't recognize the latter. Also note that we use URLs
to the raw files. This is for Edoc as it fetches the `edoc-info`
files. Edown will detect and rewrite such links in the generated
output, since "raw" links wouldn't work for the markdown files.

The next issue is that Edoc uses httpd_client to fetch the
`edoc-info` files, which requires `inets` to be started. To
further complicate matters, `ssl` (and thus `crypto` and
`public_key`) must also be started, since Github will
redirect to https.

One way to solve this is to use the escripts found under
`edown/priv/scripts`.

NOTE
====
EDoc provides a plugin structure, so that one may specify own 
layout modules, export modules, and doclets. However, there is 
some overlap esp. between the layout and doclet modules, and 
several functions are expected to produce files on their own.
This causes a problem for EDown, since it cannot handle frames.
Instead, it would probably like to create one overview file with
different sections. It would have been better to have a framework
where some plugin functions identify the different files to be 
written, and the outline of each, other plugins convert to suitable
content representation (e.g. HTML or Markdown), and EDoc then 
writes the files necessary.

For now, EDown focuses on producing reasonable Markdown, rather
than complying fully with the plugin framework. That is, the 
edown_doclet module will not go out of its way to function together
with any other layout module than edown_layout, and vice versa.

markedoc
========

The sed script bin/markedoc works in the opposite direction and converts 
your `README.md` to an `EDoc` file. 

See [bin/MARKEDOC-README.md](http://github.com/esl/edown/blob/master/bin/MARKEDOC-README.md).

**FreeBSD, Mac OS X**`$ sed -E -f markedoc.sed <markdown file> > <edoc file>`

**Linux**`$ sed -r -f markedoc.sed <markdown file> > <edoc file>`


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/esl/edown/blob/master/doc/edown_doclet.md" class="module">edown_doclet</a></td></tr>
<tr><td><a href="http://github.com/esl/edown/blob/master/doc/edown_layout.md" class="module">edown_layout</a></td></tr>
<tr><td><a href="http://github.com/esl/edown/blob/master/doc/edown_lib.md" class="module">edown_lib</a></td></tr>
<tr><td><a href="http://github.com/esl/edown/blob/master/doc/edown_make.md" class="module">edown_make</a></td></tr>
<tr><td><a href="http://github.com/esl/edown/blob/master/doc/edown_xmerl.md" class="module">edown_xmerl</a></td></tr></table>

