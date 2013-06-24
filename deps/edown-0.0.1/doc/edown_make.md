

# Module edown_make #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#from_script-1">from_script/1</a></td><td>Reads ConfigFile and calls <a href="edoc.md#application-3"><code>edoc:application/3</code></a></td></tr><tr><td valign="top"><a href="#main-1">main/1</a></td><td>Escript entry point for building edown (or edoc) documentation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="from_script-1"></a>

### from_script/1 ###


<pre><code>
from_script(Config::ConfigFile) -&gt; ok | {error, Reason}
</code></pre>

<br></br>



Reads ConfigFile and calls [`edoc:application/3`](edoc.md#application-3)



The ConfigFile will be read using [`file:script/1`](file.md#script-1), and should return
`{App, Dir, Options}`, as required by [`edoc:application/3`](edoc.md#application-3).


This function does not manage dependencies. It is simply a wrapper around
[`edoc:application/3`](edoc.md#application-3).
<a name="main-1"></a>

### main/1 ###


<pre><code>
main(Args::[Config]) -&gt; no_return()
</code></pre>

<br></br>



Escript entry point for building edown (or edoc) documentation



Usage: edown_make -config ConfigFile [-pa P] [-pz P]



Calls [from_script(ConfigFile)](#from_script-1) and then terminates,
with a normal or non-normal exit code, depending on the outcome.



Make sure `$EDOWN/edown_make` is runnable, and in the command path, and
that the edown BEAM files are in the Erlang path (e.g. using $ERL_LIBS).
The `edown_make` escript also accepts `-pa P` and/or `-pz P` flags as a
means of locating the edown byte code.



Note, however, that the function `edoc_make:main/1` only expects the
config file as an input argument, corresponding to



`escript edoc_make.beam ConfigFile`


(The reason for this is that if the beam file can be passed directly to
the escript command, setting the path should also be doable that way).

