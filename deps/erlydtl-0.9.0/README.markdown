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


Compilation
-----------

To compile ErlyDTL, run

    make

in this directory.


Template compilation
--------------------

Usage:

    erlydtl:compile_file("/path/to/template.dtl", my_module_name)

    erlydtl:compile_file("/path/to/template.dtl", my_module_name, Options)

    erlydtl:compile_template("<html>{{ foo }}</html>", my_module_name)

    erlydtl:compile_template("<html>{{ foo }}</html>", my_module_name, Options)

Result:

    ok %% existing compiled template is up to date.

    {ok, Module}
    {ok, Module, Warnings}
    {ok, Module, Binary}
    {ok, Module, Binary, Warnings}

    error
    {error, Errors, Warnings}

Options is a proplist possibly containing:

* `out_dir` - Directory to store generated .beam files. If not
  specified, no .beam files will be created and a warning is
  emitted. To silence the warning, use `{out_dir, false}`.

* `doc_root` - Included template paths will be relative to this
  directory; defaults to the compiled template's directory.

* `custom_tags_dir` - Directory of DTL files (no extension) includable
  as tags.  E.g. if $custom_tags_dir/foo contains `<b>{{ bar }}</b>`,
  then `{% foo bar=100 %}` will evaluate to `<b>100</b>`. Get it?

* `custom_tags_modules` - A list of modules to be used for handling
  custom tags. The modules will be searched in order and take
  precedence over `custom_tags_dir`. Each custom tag should correspond
  to an exported function with one of the following signatures:

      some_tag(TagVars)          -> iolist()
      some_tag(TagVars, Options) -> iolist()

  The `TagVars` are variables provided to a custom tag in the
  template's body (e.g. `{% foo bar=100 %}` results in `TagVars =
  [{bar, 100}]`).  The `Options` are options passed as the second
  argument to the `render/2` call at render-time. (These may include
  any options, not just `locale` and `translation_fun`.)

* `custom_filters_modules` - A list of modules to be used for handling
  custom filters. The modules will be searched in order and take
  precedence over the built-in filters. Each custom filter should
  correspond to an exported filter, e.g.

      some_filter(Value) -> iolist()

  If the filter takes an argument (e.g. "foo:2"), the argument will be
  also be passed in:

      some_filter(Value, Arg) -> iolist()

* `vars` - Variables (and their values) to evaluate at compile-time
  rather than render-time. (Currently not strictly true, see
  [#61](https://github.com/erlydtl/erlydtl/issues/61))

* `reader` - {module, function} tuple that takes a path to a template
  and returns a binary with the file contents. Defaults to `{file,
  read_file}`. Useful for reading templates from a network resource.

* `force_recompile` - Recompile the module even if the source's
  checksum has not changed. Useful for debugging.

* `locale` - DEPRECATED. The same as {blocktrans_locales, [Val]}.

* `blocktrans_fun` - A two-argument fun to use for translating
  `blocktrans` blocks, `trans` tags and `_(..)` expressions. This will
  be called once for each pair of translated element and locale
  specified in `blocktrans_locales`. The fun should take the form:

      Fun(Block::string(), Locale::string()) -> <<"ErlyDTL code">>::binary() | default

* `blocktrans_locales` - A list of locales to be passed to
  `blocktrans_fun`.  Defaults to [].

* `binary_strings` - Whether to compile strings as binary terms
  (rather than lists). Defaults to `true`.

* `record_info` - List of records to look for when rendering the
  template. Each record info is a tuple with the fields of the record:

      {my_record, record_info(fields, my_record)}

* `no_env` - Do not read additional options from the OS environment
  variable `ERLYDTL_COMPILER_OPTIONS`.

* `auto_escape` - Control automatic HTML escaping of template
  values. Enabled by default.

* `no_load` - Do not load the compiled template.

* `binary` - Include the compiled template binary code in the result
  tuple (between the module name and any warning/error lists). Note,
  this option is named the same as for the Erlang compiler, with
  similar use, except that this option does NOT affect whether or not
  a .beam file is saved.

* `return` - Short form for both `return_warnings` and `return_errors`.

* `return_warnings` - If this flag is set, then an extra field
  containing warnings is added to the tuple returned on success.

* `return_errors` - If this flag is set, then an error-tuple with two
  extra fields containing errors and warnings is returned when there
  are errors.

* `report` - Short form for both `report_warnings` and `report_errors`.

* `report_warnings` - Print warnings as they occur.

* `report_errors` - Print errors as they occur.

* `warnings_as_errors` - Treat warnings as errors.

* `verbose` - Enable verbose printing of compilation results.

* `compiler_options` - Proplist with extra options passed directly to
  `compiler:forms/2`. This can prove useful when using extensions to
  add extra defines etc when compiling the generated code.


Helper compilation
------------------

Helpers provide additional templating functionality and can be used in
conjunction with the `custom_tags_module` option above. They can be
created from a directory of templates thusly:

    erlydtl:compile_dir("/path/to/dir", my_helper_module_name)

    erlydtl:compile_dir("/path/to/dir", my_helper_module_name, Options)

The resulting module will export a function for each template
appearing in the specified directory. Options is the same as for
`compile/3`.

Compiling a helper module can be more efficient than using
`custom_tags_dir` because the helper functions will be compiled only
once (rather than once per template).


Usage (of a compiled template)
------------------------------

    my_compiled_template:render(Variables) -> {ok, IOList} | {error, Err}

Variables is a proplist, dict, gb_tree, or a parameterized module
(whose method names correspond to variable names). The variable values
can be atoms, strings, binaries, or (nested) variables.

IOList is the rendered template.

    my_compiled_template:render(Variables, Options) ->
            {ok, IOList} | {error, Err}

Same as `render/1`, but with the following options:

* `translation_fun` - A fun/1 that will be used to translate strings
  appearing inside `{% trans %}` and `{% blocktrans %}` tags. The simplest
  TranslationFun would be `fun(Val) -> Val end`. Placeholders for
  blocktrans variable interpolation should be wrapped to `{{` and `}}`.

* `locale` - A string specifying the current locale, for use with the
  `blocktrans_fun` compile-time option.

```erlang
my_compiled_template:translatable_strings() -> [String]
```

List of strings appearing in `{% trans %}` and `_(..)` tags.

    my_compiled_template:translated_blocks() -> [String]

List of strings appearing in `{% blocktrans %}...{% endblocktrans %}`
blocks; the translations (which can contain ErlyDTL code) are
hard-coded into the module and appear at render-time. To get a list of
translatable blocks before compile-time, use the provided
`blocktrans_extractor` module.

    my_compiled_template:source() -> {FileName, CheckSum}

Name and checksum of the original template file.

    my_compiled_template:dependencies() -> [{FileName, CheckSum}]

List of names/checksums of templates included by the original template
file. Useful for frameworks that recompile a template only when the
template's dependencies change.

    my_compiled_template:variables() -> [Variable::atom()]

Sorted list of unique variables used in the template's body. The list
can be used for determining which variable bindings need to be passed
to the render/3 function.


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
* `regroup` requires a closing `endregroup` tag. See
  [Issue #101](https://github.com/erlydtl/erlydtl/issues/101).


Tests
-----

From a Unix shell, run:

    make test

Note that the tests will create some output in tests/output.


[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/erlydtl/erlydtl/trend.png)](https://bitdeli.com/free "Bitdeli Badge")
