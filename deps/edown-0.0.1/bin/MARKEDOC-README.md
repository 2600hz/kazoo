edown markedoc 0.3.2
====================

**markedoc helps you keep your project's README.md in sync with your overview.edoc.**

This is the opposite direction from what **edown** otherwise does.

markedoc translates [Markdown][] formatted texts into [Erlang][] [EDoc][] format, for inclusion into [EDoc][] generated html docs. It is for use on Linux, FreeBSD and Mac OS X and any system that you can install  **[sed][Requirements]** on.

Status: [pre-beta][Status]. Quite stable and usable. See [Status][].

markedoc is a mere [sed][] command file to convert markdown to edoc. It is part of the **[edown][]** project. The actual script file is in the bin folder: bin/markedoc.sed. Your contribution to make markedoc stable is highly [welcome][issues].

[issues]: https://github.com/hdiedrich/markedoc/issues "Issue tracker"

Use                                                           <a name=Use></a>
---
At the command line for

**FreeBSD, Mac OS X**
	$ sed -E -f markedoc.sed <markdown file> > <edoc file>

**Linux**
	$ sed -r -f markedoc.sed <markdown file> > <edoc file>

Usage for Linux and FreeBSD and Mac OS X is completely the same, except for the -r instead of the -E parameter. Both mean the same but happen to have a different name. In the examples below, replace -E with -r where necessary.

Requirements                                          <a name=Requirements></a>
------------
* **[sed][]**: is part of any Linux, FreeBSD and Mac OSX distribution, also see [Notes][].

* **[Erlang/OTP][Erlang]**, see [Notes][].

Test                                                          <a name=Test></a>
----

 **FreeBSD, Mac OS X**
	$ samples/markedoc/test-bsd.sh

 **Linux**
	$ samples/markedoc/test-linux.sh

Then check html files as listed in the output.

Sample                                                      <a name=Sample></a>
------

From edown project root, try out:

 **FreeBSD, Mac OS X**
	$ sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE1.md > samples/markedoc/doc/SAMPLE.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[]'

 **Linux**
	$ sed -r -f bin/markedoc.sed samples/markedoc/SAMPLE1.md > samples/markedoc/doc/SAMPLE.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[]'

This creates a SAMPLE.edoc file from SAMPLE1.md, which is then included in the EDoc generation. Point your browser at

	samples/markedoc/doc/overview-summary.html

to see the result. For something only vaguely related but pretty, try:

	$ erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'

This illustrates the motivation for the markedoc as it is now: to have all code lines in one block in order to be able to address them as one united div from css.		

For your own projects you'd copy markedoc.sed in the right place and do something like:

 **FreeBSD, Mac OS X**
	$ sed -E -f bin/markedoc.sed README.md > doc/README.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"."' '[]'	

 **Linux**
	$ sed -r -f bin/markedoc.sed README.md > doc/README.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"."' '[]'	

And that's it. This could also be part of your Makefile. For the intermediary README.edoc to automatically become part of your generated EDoc html pages, you would use a @docfile tag in your overview.edoc file, like so:

	@docfile "doc/README.edoc"

By running sed, then edoc, this makes the README.edoc part of the overview page. You could also make the README.md straight into an overview.edoc but the way it is allows allows to embedd it into additional context information that should be useful for a proper html doc.

Accordingly, the sample stub overview.edoc used for the samples here, looks like this:

	@author You 
	@title  a markedoc sample doc
	@version 0.2
	@docfile "samples/markedoc/doc/SAMPLE.edoc"

Tricks                                                       <a name=Tricks></a>
------

Markdown cannot jump to headlines as anchors, while edoc makes headlines into anchors automatically. To allow for meaningful anchor jumps like [sample][] within a page, the following workaround makes sense. It is 'weeded out' by markedoc so that it does not trip up edoc. But it makes for local jumps in
both worlds:

	## Examples                                     <a name=example></a>
	
	...
	
	[sample]: #sample


This makes a tag `[example][]' into a direct jump to the headline 'Example', in both markdown and edoc. 
Markdown actually uses the `[sample]: #sample' reference. EDoc, however, automatically inserts an anchor for 'Example' being a headline, and of the same name. (The links are not case sensitive.) 
If you get the reference wrong or forget to make it, the link tag will be displayed in the open, as actual `[example][]'.


Status                                                       <a name=Status></a>
------

 **Pre-Beta**. Quite usable, but still likes to trip up EDoc now and then, which is kind of easy to do. 

There are  many ways to create formats that will make the EDoc generator tilt and unfortunately, the errors it throws are sometimes not quite so illuminating to the reader. But why not try an incremental approach and see what works. As you can see from this [source sample][sample], which works alright, it's quite a lot that *does* work and the murky bits can usally be worked out fast. Sometimes an additional line helps, some spaces at the end of a line, general intuitive stuff. Please experiment and push your fixes to me.

 **Thanks!**

Notes                                                         <a name=Notes></a>
-----

 **[Erlang][]** is a programming language used to build massively scalable soft real-time systems with requirements on high availability. Some of its uses are in telecom, banking, e-commerce, computer telephony and instant messaging. Erlang's runtime system has built-in support for concurrency, distribution and fault tolerance. Erlang comes bundled with the Open Telecom Platform, OTP.

[Erlang]: http://www.erlang.org/doc/  

 **[EDoc][]** is the Erlang program documentation generator. Inspired by the Javadoc tool for the Java programming language, EDoc is adapted to the conventions of the Erlang world, and has several features not found in Javadoc. Edoc is part of the Erlang/OTP distribution.

[EDoc]: http://www.erlang.org/doc/apps/edoc/chapter.html

 **[edown][]** is an EDoc extension for generating Github-flavored Markdown. It uses edoc-style commented Erlang sources to create markdown files from them. 

[edown]: https://github.com/esl/edown

 **[Markdown][]** is a text-to-HTML conversion tool for web writers. Markdown allows you to write using an easy-to-read, easy-to-write plain text format, then convert it to structurally valid XHTML (or HTML).

[Markdown]: http://daringfireball.net/projects/markdown/ 

 **[sed][]** ('stream editor') is a Unix utility that parses text files and implements a programming language which can apply textual transformations to such files. It reads input files line by line (sequentially), applying the operation which has been specified via the command line (or a sed script), and then outputs the line. It is available today for most operating systems. There seems to be [one for Windows][winsed], too.

[sed]: http://en.wikipedia.org/wiki/Sed
[winsed]: http://gnuwin32.sourceforge.net/packages/sed.htm
[sample]: https://github.com/Eonblast/Emysql/raw/master/README.md "This markdown file is translated alright by markedoc."


License
-------
This script is free software. It comes without any warranty.

Author
------
H. Diedrich <hd2010@eonblast.com>

History
-------

02/18/11 - 0.3.2 - **edown**

* integrated into edown

02/05/11 - 0.3.1 - **more polish** - Linux, FreeBSD, Mac OS X

* added weeding out of markdown anchor references (an md workaround)
* added protection for & (but edoc still only accepts number codes)
* fixed trip up by trailing spaces in underline headline format 
* checked commented out alternate code for code blocks and references.

02/03/11 - 0.3 - **rough edges polished** - Linux, FreeBSD, Mac OS X

* added doc for Linux use
* added support for multi-line '[..]: ... "..."' references
* added footnote signs and sepcial chars:
* dagger, double dagger: (+), (++), stars: (\*), (\*\*), (\*\*\*)  
* superscript 1, 2, 3: (\*1), (\*2), (\*3), copyright (C), (R), (TM),  
* guillemots <<, >> and middle dot ::
* added test batches etc/test-bsd.sh and etc/test-linux.sh
* added css sample in samples/markedoc/what-you-could-see/ 
* added classes for ``< li >'' list item tags for '[..]:...'-references
* fixed italic and bold merker interference bullet points
* eliminated [..]: part of '[..]:...'-references, flipping "..." to lead
* dev: sample creation batch make_samples.sh added
	
02/02/11 - 0.2 - **basics complete** - FreeBSD / Mac OS X

* added support for === and --- headline format
* fixed cutting off of last lines 
* fixed page-local anchor jumps
* fixed space in javascript links
* eliminated end-space requirement at end of '[..]:...'-style references.
* eliminated need for echoing '@doc' first into edoc output file
* added javascript title tag setting for '[..]:...'-style references.
	
01/31/11 - 0.1 - **first release:** FreeBSD / Mac OS X
	
[Requirements]: #Requirements
[Status]: #Status
[Notes]: #Notes
[Test]: #Test
