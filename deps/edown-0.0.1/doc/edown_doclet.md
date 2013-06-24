

# Module edown_doclet #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


EDoc Doclet module for producing Markdown.
Copyright (c) 2010 Erlang Solutions Ltd

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-2">run/2</a></td><td>Main doclet entry point.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="run-2"></a>

### run/2 ###


<pre><code>
run(Command::<a href="#type-doclet_gen">doclet_gen()</a> | <a href="#type-doclet_toc">doclet_toc()</a>, Ctxt::<a href="#type-edoc_context">edoc_context()</a>) -&gt; ok
</code></pre>

<br></br>



Main doclet entry point.



Also see [`//edoc/edoc:layout/2`](http://www.erlang.org/doc/man/edoc.html#layout-2) for layout-related options, and
[`//edoc/edoc:get_doc/2`](http://www.erlang.org/doc/man/edoc.html#get_doc-2) for options related to reading source
files.


Options:



<dt><code>{file_suffix, string()}</code>
</dt>




<dd>Specifies the suffix used for output files. The default value is
<code>".md"</code>.
</dd>




<dt><code>{hidden, bool()}</code>
</dt>




<dd>If the value is <code>true</code>, documentation of hidden modules and
functions will also be included. The default value is <code>false</code>.
</dd>




<dt><code>{overview, <a href="http://www.erlang.org/doc/man/edoc.html#type-filename">//edoc/edoc:filename()</a>}</code>
</dt>




<dd>Specifies the name of the overview-file. By default, this doclet
looks for a file <code>"overview.edoc"</code> in the target directory.
</dd>




<dt><code>{private, bool()}</code>
</dt>




<dd>If the value is <code>true</code>, documentation of private modules and
functions will also be included. The default value is <code>false</code>.
</dd>




<dt><code>{stylesheet, string()}</code>
</dt>




<dd>Specifies the URI used for referencing the stylesheet. The
default value is <code>"stylesheet.css"</code>. If an empty string is
specified, no stylesheet reference will be generated.
</dd>




<dt><code>{stylesheet_file, <a href="http://www.erlang.org/doc/man/edoc.html#type-filename">//edoc/edoc:filename()</a>}</code>
</dt>




<dd>Specifies the name of the stylesheet file. By default, this
doclet uses the file <code>"stylesheet.css"</code> in the <code>priv</code>
subdirectory of the EDoc installation directory. The named file
will be copied to the target directory.
</dd>




<dt><code>{title, string()}</code>
</dt>




<dd>Specifies the title of the overview-page.
</dd>



