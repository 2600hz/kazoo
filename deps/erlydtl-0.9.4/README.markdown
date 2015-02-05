ErlyDTL [![Build Status](https://travis-ci.org/erlydtl/erlydtl.png?branch=master)](https://travis-ci.org/erlydtl/erlydtl)
=======

ErlyDTL compiles Django Template Language to Erlang bytecode.

Project homepage: <https://github.com/erlydtl/erlydtl/wiki>

ErlyDTL implements the Django Template Language as documented for
version *1.6*, here:
<http://docs.djangoproject.com/en/1.6/ref/templates/builtins/>

Despite our best efforts to be completely compatible with the Django
Template Languge, there are still a few
[differences](https://github.com/erlydtl/erlydtl#differences-from-standard-django-template-language).


### The erlydtl branches & tags

As things are progressing somewhat more rapidly, I'll describe our
branch and tagging strategy to help you stay on the branch or tag(s)
that suits you best.

#### master branch

This is were all the action is, and at times may be slightly
broken. Suitable for early adopters who aren't afraid of a little
debugging and hopefully also reporting issues.

#### release tags

Whenever *master* is deemed stable with a nice set of
additions/changes, it is merged with stable for a new release.

As we're still going for the big 1.0 release, breaking changes **may**
be introduced also on minor release bumps; but after that, we'll stick
with [semver](http://semver.org/) (as a matter of fact, this is all
covered by the Semantic Versioning Spec 2.0.0, see 4§).

#### stable branch

Releases are made from the *stable* branch, with dependency versions
pinned down and a hard coded version number in the app file.


Compilation
-----------

To compile ErlyDTL, run

    make

in this directory.


#### Do not use Erlang R16B03

The erl syntax tools is broken in Erlang R16B03, use R16B03-1 or any
other supported version instead.


Template compilation
--------------------

Usage:

```erlang
erlydtl:compile_file("/path/to/template.dtl", my_module_name)

erlydtl:compile_file("/path/to/template.dtl", my_module_name, Options)

erlydtl:compile_template("<html>{{ foo }}</html>", my_module_name)

erlydtl:compile_template("<html>{{ foo }}</html>", my_module_name, Options)
```

Result:

```erlang
{ok, Module}
{ok, Module, Warnings}
{ok, Module, Binary}
{ok, Module, Binary, Warnings}

error
{error, Errors, Warnings}
```

Options is a proplist possibly containing:

* `auto_escape` - Control automatic HTML escaping of template
  values. Enabled by default.

* `binary` - Include the compiled template binary code in the result
  tuple (between the module name and any warning/error lists). Note,
  this option is named the same as for the Erlang compiler, with
  similar use, except that this option does NOT affect whether or not
  a .beam file is saved.

* `binary_strings` - Whether to compile strings as binary terms
  (rather than lists). Defaults to `true`.

* `compiler_options` - Proplist with extra options passed directly to
  `compiler:forms/2`. This can prove useful when using extensions to
  add extra defines etc when compiling the generated code.

* `constants` - Replace template variables with a constant value when
  compiling the template. This can _not_ be overridden when rendering
  the template. See also `default_vars`.

* `custom_filters_modules` **deprecated** - A list of modules to be
  used for handling custom filters. The modules will be searched in
  order and take precedence over the built-in filters. Each custom
  filter should correspond to an exported filter, e.g.

  ```erlang
  some_filter(Value) -> iolist()
  ```

  If the filter takes any arguments (e.g. "foo:2"), those will be
  added to the call:

  ```erlang
  some_filter(Value, Arg) -> iolist()
  ```

* `custom_tags_dir` - Directory of DTL files (no extension) includable
  as tags.  E.g. if `$custom_tags_dir/foo` contains `<b>{{ bar
  }}</b>`, then `{% foo bar=100 %}` will evaluate to `<b>100</b>`.

* `custom_tags_modules` **deprecated** - A list of modules to be used
  for handling custom tags. The modules will be searched in order and
  take precedence over `custom_tags_dir`. Each custom tag should
  correspond to an exported function with one of the following
  signatures:

  ```erlang
  some_tag(TagVars)          -> iolist()
  some_tag(TagVars, Options) -> iolist()
  ```

  The `TagVars` are variables provided to a custom tag in the
  template's body (e.g. `{% foo bar=100 %}` results in `TagVars =
  [{bar, 100}]`).  The `Options` are options passed as the second
  argument to the `render/2` call at render-time. (These may include
  any options, not just `locale` and `translation_fun`.)

* `debug_compiler` - Enable compiler debug diagnostics.  Currently it
  debug prints the options passed to `compile:forms` (i.e. if
  verbosity is >= 2; that is, with two or more `verbose` options) and
  enables the saving of the compiled template in source form to a .erl
  file.

* `debug_info` - This option is passed to `compile:forms` to include
  debug information in the compiled module.

* `debug_root` - Only applies when `debug_compiler` is `true`. The
  root directory for debug source dumps. If set to `false`, no source
  dump will be saved. Defaults to `undefined`, leaving the source dump
  next to the source template file.

* `default_libraries` - A list of libraries that should be loaded by
  default when compiling a template. Libraries can be specified either
  by name (when there is a name to module mapping also provided in the
  `libraries` option) or by module.

* `default_vars` - Provide default values for variables. Any value
  from the render variables takes precedence. Notice: in case the
  value is a `fun/0`, it will be called at compile time. See also
  `constants`.

* `doc_root` - Included template paths will be relative to this
  directory; defaults to the compiled template's directory.

* `extension_module` **experimental** - This is work in progress to
  make erlydtl extensible.

* `force_recompile` - Recompile the module even if the source's
  checksum has not changed. Useful for debugging.

* `libraries` - A list of `{Name, Module}` libraries implementing
  custom tags and filters. `Module` should implement the
  `erlydtl_library` behaviour (see [Custom tags and filters] below).

* `lists_0_based` - **Compatibility warning** Defaults to `false`,
  giving 1-based list access, as is common practice in Erlang. Set it
  to `true` to get 1-based access as in Django, or to `defer` to not
  decide until render time, using the render option
  `lists_0_based`. See also `tuples_0_based`.

* `locale` - Locale to translate to during compile time. May be
  specified multiple times as well as together with the `locales`
  option.

* `locales` - A list of locales to be passed to `translation_fun`.
  Defaults to [].

* `no_env` - Do not read additional options from the OS environment
  variable `ERLYDTL_COMPILER_OPTIONS`.

* `no_load` - Do not load the compiled template.

* `out_dir` - Directory to store generated .beam files. If not
  specified, no .beam files will be created and a warning is
  emitted. To silence the warning, use `{out_dir, false}`.

* `reader` - {module, function} tuple that takes a path to a template
  and returns a binary with the file contents. Defaults to `{file,
  read_file}`. Useful for reading templates from a network resource.

* `record_info` - List of records to look for when rendering the
  template. Each record info is a tuple with the fields of the record:

  ```erlang
  {my_record, record_info(fields, my_record)}
  ```

* `return` - Short form for both `return_warnings` and `return_errors`.

* `return_warnings` - If this flag is set, then an extra field
  containing warnings is added to the tuple returned on success.

* `return_errors` - If this flag is set, then an error-tuple with two
  extra fields containing errors and warnings is returned when there
  are errors.

* `report` - Short form for both `report_warnings` and `report_errors`.

* `report_warnings` - Print warnings as they occur.

* `report_errors` - Print errors as they occur.

* `translation_fun` - A two-argument fun to use for translating
  `blocktrans` blocks, `trans` tags and `_(..)` expressions at compile
  time. This will be called once for each pair of translated element
  and locale specified with `locales` and `locale` options. The fun
  should take the form:

  ```erlang
  fun (Block::string(), Locale|{Locale, Context}) ->
      <<"ErlyDTL code">>::binary() | default
    when Locale::string(), Context::string().
  ```

  See description of the `translation_fun` render option for more
  details on the translation `context`.

  Notice, you may instead pass a `fun/0`, `{Module, Function}` or
  `{Module, Function, Args}` which will be called recursively until it
  yields a valid translation function, at which time any needed
  translation setup actions can be carried out prior to returning the
  next step (either another setup function/tuple, or the translation
  function).

  ```erlang
  %% sample translation setup
  fun () ->
      translation_engine:init(),
      fun translation_engine:translate/2
  end
  ```

* `tuples_0_based` - **Compatibility warning** Defaults to `false`,
  giving 1-based tuple access, as is common practice in Erlang. Set it
  to `true` to get 1-based access as in Django, or to `defer` to not
  decide until render time, using the render option
  `tuples_0_based`. See also `lists_0_based`.


* `vars` **deprecated** - Use `default_vars` instead. Variables (and
  their values) to evaluate at compile-time rather than
  render-time.


* `verbose` - Enable verbose printing of compilation progress. Add
  several for even more verbose (e.g. debug) output.

* `warnings_as_errors` - Treat warnings as errors.


Helper compilation
------------------

Helpers provide additional templating functionality and can be used in
conjunction with the `custom_tags_module` option above. They can be
created from a directory of templates thusly:

```erlang
erlydtl:compile_dir("/path/to/dir", my_helper_module_name)

erlydtl:compile_dir("/path/to/dir", my_helper_module_name, Options)
```

The resulting module will export a function for each template
appearing in the specified directory. Options is the same as for
`compile/3`.

Compiling a helper module can be more efficient than using
`custom_tags_dir` because the helper functions will be compiled only
once (rather than once per template).

Notice: The exported template functions return an `iolist()` on
success only, failures are non-local (e.g. as a throw). To get the
result in wrapped tuple `{ok, iolist()} | {error, Reason}` call one of
the `render` functions: `render(Tag) | render(Tag, Vars) | render(Tag,
Vars, Opts)`.


Usage (of a compiled template)
------------------------------


### render/1

```erlang
my_compiled_template:render(Variables) -> {ok, IOList} | {error, Err}
```

Variables is a proplist, dict, gb_tree, or a parameterized module
(whose method names correspond to variable names). The variable values
can be atoms, strings, binaries, or (nested) variables.

IOList is the rendered template.


### render/2

```erlang
my_compiled_template:render(Variables, Options) -> {ok, IOList} | {error, Err}
```

Same as `render/1`, but with the following options:

* `translation_fun` - A `fun/1` or `fun/2` that will be used to
  translate strings appearing inside `{% trans %}` and `{% blocktrans
  %}` tags at render-time. The simplest TranslationFun would be
  `fun(Val) -> Val end`. Placeholders for blocktrans variable
  interpolation should be wrapped in `{{` and `}}`. In case of
  `fun/2`, the extra argument is the current locale, possibly together
  with a translation context in a tuple:

  ```erlang
  fun (Val|{Val, {Plural_Val, Count}}, Locale|{Locale, Context}) ->
      Translated_Val
  end
  ```

  The context is present when specified in the translation
  tag. Example:

  ```django
  {% trans "Some text to translate" context "app-specific" %}
    or
  {% blocktrans context "another-context" %}
    Translate this for {{ name }}.
  {% endblocktrans %}
  ```

  The plural form is present when using `count` and `plural` in a
  `blocktrans` block:

  ```django
  {% blocktrans count counter=var|length %}
    There is {{ counter }} element in the list.
  {% plural %}
    There are {{ counter }} elements in the list.
  {% endblocktrans %}
  ```

  Notice, the translation fun can also be a `fun/0` or a MFA-tuple to
  setup the translation prior to rendering. See the `translation_fun`
  compile option for more details.

* `lists_0_based` - If the compile option `lists_0_based` was set to
  `defer`, pass this option (or set it to true, `{lists_0_based,
  true}`) to get 0-based list indexing when rendering the
  template. See also `tuples_0_based`.

* `locale` - A string specifying the current locale, for use with the
  `translation_fun` compile-time option.

* `tuples_0_based` - If the compile option `tuples_0_based` was set to
  `defer`, pass this option (or set it to true, `{tuples_0_based,
  true}`) to get 0-based tuple indexing when rendering the
  template. See also `lists_0_based`.


### translatable_strings/0

```erlang
my_compiled_template:translatable_strings() -> [String]
```

List of strings appearing in `{% trans %}` and `_(..)` tags.


### translated_blocks/0

```erlang
my_compiled_template:translated_blocks() -> [String]
```

List of strings appearing in `{% blocktrans %}...{% endblocktrans %}`
blocks; the translations (which can contain ErlyDTL code) are
hard-coded into the module and appear at render-time. To get a list of
translatable blocks before compile-time, use the provided
`blocktrans_extractor` module.


### source/0

```erlang
my_compiled_template:source() -> {FileName, CheckSum}
```

Name and checksum of the original template file.


### dependencies/0

```erlang
my_compiled_template:dependencies() -> [{FileName, CheckSum}]
```

List of names/checksums of templates included by the original template
file. Useful for frameworks that recompile a template only when the
template's dependencies change.


### variables/0

```erlang
my_compiled_template:variables() -> [Variable::atom()]
```

Sorted list of unique variables used in the template's body. The list
can be used for determining which variable bindings need to be passed
to the `render/3` function.


### default_variables/0

```erlang
my_compiled_template:default_variables() -> [Variable::atom()]
```

Like `variables/0`, but for any variable which have a default value
provided at compile time.


### constants/0

```erlang
my_compiled_template:constants() -> [Variable::atom()]
```

Like `default_variables/0`, but these template variables has been
replaced with a fixed value at compile time and can not be changed
when rendering the template.


Custom tags and filters
-----------------------

Starting with release *0.9.1*, the recommended way to add custom tags
and filters are to register a module implementing the
`erlydtl_library` behaviour. There are two functions needed to
implement a custom library: `version/0` and `inventory/1`.

The `version/0` function is to be able to keep backwards compatibility
in face of an evolving library behaviour, and should return the
version of the behaviour the library supports. The valid range of
versions is in the spec for the `version/0` callback in
`erlydtl_library.erl`.

### Library version 1

The `inventory/1` function is called with either `filters` or `tags`
as argument, and should return a list of all filters or tags available
in that library, respectively (see spec in `erlydtl_library.erl` for
details on syntax for this list).

Tag functions must take a list of arguments, and may take a list of
render options, and should return an `iolist()` as result value (see
`custom_tags_modules` compile option).

Filter functions must take an `iolist()` value to filter, and may take
an argument, and should return an `iolist()` as result value (see
`custom_filters_modules` compile option).


Differences from standard Django Template Language
--------------------------------------------------

* `csrf_token` The
  [Cross Site Request Forgery](https://docs.djangoproject.com/en/1.6/ref/contrib/csrf/)
  tag is not implemented.
* `url` The
  [url](https://docs.djangoproject.com/en/1.6/ref/templates/builtins/#url)
  tag is not implemented. This should be
  [addressed](https://github.com/erlydtl/erlydtl/issues/115) in a
  future release.
* List indexing is 1-based in erlydtl, while 0-based in Django (see
  [#156](https://github.com/erlydtl/erlydtl/issues/156)).
* For an up-to-date list, see all
  [issues](https://github.com/erlydtl/erlydtl/issues) marked
  `dtl_compat`.
* Erlang specifics: Template variables may be prefixed with underscore
  (`_`) to avoid "unused variable" warnings (see
  [#164](https://github.com/erlydtl/erlydtl/issues/164)).


Tests
-----

From a Unix shell, run:

    make tests

Note that the tests will create some output in tests/output in case of regressions.
