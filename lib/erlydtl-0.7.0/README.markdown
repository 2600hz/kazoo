ErlyDTL
=======

ErlyDTL compiles Django Template Language to Erlang bytecode.

*Supported tags*: autoescape, block, comment, cycle, extends, filter, firstof, for, if, ifequal, ifnotequal, include, now, spaceless, ssi, templatetag, trans, widthratio, with

_Unsupported tags_: csrf_token, ifchanged, regroup, url

*Supported filters*: add, addslashes, capfirst, center, cut, date, default, default_if_none, dictsort, dictsortreversed, divisibleby, escape, escapejs, filesizeformat, first, fix_ampersands, floatformat, force_escape, format_integer, format_number, get_digit, iriencode, join, last, length, length_is, linebreaks, linebreaksbr, linenumbers, ljust, lower, make_list, phonenumeric, pluralize, pprint, random, random_num, random_range, removetags, rjust, safe, safeseq, slice, slugify, stringformat, striptags, time, timesince, timeuntil, title, truncatewords, truncatewords_html, unordered_list, upper, urlencode, urlize, urlizetrunc, wordcount, wordwrap, yesno

_Unsupported filters_: _none_

Project homepage: <http://code.google.com/p/erlydtl/>

Language reference: <http://docs.djangoproject.com/en/dev/ref/templates/builtins/>


Compilation
-----------

To compile ErlyDTL, run 

    make
    
in this directory.


Template compilation
--------------------

Four ways:

    erlydtl:compile("/path/to/template.dtl", my_module_name)

    erlydtl:compile("/path/to/template.dtl", my_module_name, Options)

    erlydtl:compile(<<"<html>{{ foo }}</html>">>, my_module_name)

    erlydtl:compile(<<"<html>{{ foo }}</html>">>, my_module_name, Options)

Options is a proplist possibly containing:

* `out_dir` - Directory to store generated .beam files. If not specified, no
.beam files will be created.

* `doc_root` - Included template paths will be relative to this directory;
defaults to the compiled template's directory.

* `custom_tags_dir` - Directory of DTL files (no extension) includable as tags.
E.g. if $custom_tags_dir/foo contains `<b>{{ bar }}</b>`, then `{% foo bar=100 %}` 
will evaluate to `<b>100</b>`. Get it?

* `custom_tags_module` - A module to be used for handling custom tags. Each custom
tag should correspond to an exported function, e.g.: 

    some_tag(Variables, TranslationFun) -> iolist()

* `vars` - Variables (and their values) to evaluate at compile-time rather than
render-time. 

* `reader` - {module, function} tuple that takes a path to a template and returns
a binary with the file contents. Defaults to `{file, read_file}`. Useful
for reading templates from a network resource.

* `compiler_options` - Proplist passed directly to `compiler:forms/2`

* `force_recompile` - Recompile the module even if the source's checksum has not
changed. Useful for debugging.

* `locale` - The locale used for template compile. Requires erlang_gettext. It
will ask gettext_server for the string value on the provided locale.
For example, adding {locale, "en_US"} will call {key2str, Key, "en_US"}
for all string marked as trans (`{% trans "StringValue" %}` on templates).
See README_I18N.


Helper compilation
------------------

Helpers provide additional templating functionality and can be used in
conjunction with the `custom_tags_module` option above. They can be created
from a directory of templates thusly:

    erlydtl:compile_dir("/path/to/dir", my_helper_module_name)
    
    erlydtl:compile_dir("/path/to/dir", my_helper_module_name, Options)

The resulting module will export a function for each template appearing
in the specified directory. Options is the same as for compile/3.

Compiling a helper module can be more efficient than using `custom_tags_dir`
because the helper functions will be compiled only once (rather than once
per template).


Usage (of a compiled template)
------------------------------ 

    my_compiled_template:render(Variables) -> {ok, IOList} | {error, Err}

Variables is a proplist, dict, gb_tree, or a parameterized module
(whose method names correspond to variable names). The variable 
values can be atoms, strings, binaries, or (nested) variables.

IOList is the rendered template.

    my_compiled_template:render(Variables, TranslationFun) -> 
            {ok, IOList} | {error, Err}

Same as `render/1`, but TranslationFun is a fun/1 that will be used to 
translate strings appearing inside `{% trans %}` tags. The simplest
TranslationFun would be `fun(Val) -> Val end`

    my_compiled_template:translatable_strings() -> [String]

List of strings appearing in `{% trans %}` tags that can be overridden 
with a dictionary passed to `render/2`.

    my_compiled_template:source() -> {FileName, CheckSum}

Name and checksum of the original template file.

    my_compiled_template:dependencies() -> [{FileName, CheckSum}]

List of names/checksums of templates included by the original template
file. Useful for frameworks that recompile a template only when the
template's dependencies change.


Tests
-----

From a Unix shell, run:

    make test

Note that the tests will create some output in tests/output.
