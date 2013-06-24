SAMPLE 3: markedoc 0.3 README of Feb 2011
=========================================

```
 --------------------------------------------------------------
| THIS TEXT IS USED AS A SAMPLE TO ILLUSTRATE MARKEDOC USAGE.  |
| If you see this in your browser, you succeeded compiling it  |
| from markdown into an edoc.  As you see it's complex enough. |
 --------------------------------------------------------------
'''

 **markedoc helps you keep your project's README.md in sync with your overview.edoc.**
 
It is for use on Linux, FreeBSD and Mac OS X and any system that you can install  **[sed][Requirements]** on.

Status: [pre-beta][Status]. Quite stable and usable. See [Status][].

markedoc translates [Markdown][] formatted texts into [Erlang][] [EDoc][] format, for inclusion into [EDoc][] generated html docs.

The actual script file is in the bin folder: bin/markedoc.sed.

markedoc is a mere [sed][] command file to convert markdown to edoc. It is part of the **[edown][]** project.

Your contribution to make markedoc stable is highly [welcome][issues].

[issues]: https://github.com/hdiedrich/markedoc/issues "Issue tracker"

Use
---
At the command line for

**FreeBSD, Mac OS X**
	$ sed -E -f markedoc.sed <markdown file> > <edoc file>

**Linux**
	$ sed -r -f markedoc.sed <markdown file> > <edoc file>

Usage for Linux and FreeBSD and Mac OS X is completely the same, except for the -r instead of the -E parameter. Both mean the same but happen to have a different name. In the examples below, replace -E with -r where necessary.

Requirements
------------
* **[sed][]**: is part of any Linux, FreeBSD and Mac OSX distribution, also see [Notes][].

* **[Erlang/OTP][Erlang]**, see [Notes][].

Test
----

 **FreeBSD, Mac OS X**
	$ etc/test-bsd.sh

 **Linux**
	$ etc/test-linux.sh

Then check html files as listed in the output.

Sample
------

From project root (were the README.md file is), try out:

 **FreeBSD, Mac OS X**
	$ sed -E -f bin/markedoc.sed samples/SAMPLE1.md > samples/doc/SAMPLE.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'

 **Linux**
	$ sed -r -f bin/markedoc.sed samples/SAMPLE1.md > samples/doc/SAMPLE.edoc
	$ erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'

This creates a SAMPLE.edoc file from SAMPLE1.md, which is then included in the EDoc generation. Point your browser at

	samples/doc/overview-summary.html

to see the result. For something only vaguely related but pretty, try:

	$ erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'

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
	@docfile "samples/doc/SAMPLE.edoc"

Status
------

 **Pre-Beta**. Quite usable, but still likes to trip up EDoc now and then, which is kind of easy to do. 

There are  many ways to create formats that will make the EDoc generator tilt and unfortunately, the errors it throws are sometimes not quite so illuminating to the reader. But why not try an incremental approach and see what works. As you can see from this [source sample][sample], which works alright, it's quite a lot that *does* work and the murky bits can usally be worked out fast. Sometimes an additional line helps, some spaces at the end of a line, general intuitive stuff. Please experiment and push your fixes to me.

 **Thanks!**

Notes
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

Todo
----
* make work with non-FreeBSD sed
* robust alternates not tested for some time
* protect ampersands

Development
-----------
To test markedoc, see '[Test][]', above. Or use

 **FreeBSD**
	sed -E -f bin/markedoc.sed samples/SAMPLE1.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
	mv samples/doc/overview-summary.html samples/your-test-results/sample1.html
	mv samples/doc/SAMPLE.edoc samples/your-test-results/SAMPLE1.edoc	
	
	sed -E -f bin/markedoc.sed samples/SAMPLE2.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'
	mv samples/doc/overview-summary.html samples/your-test-results/sample2.html
	mv samples/doc/SAMPLE.edoc samples/your-test-results/SAMPLE2.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE3.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
	mv samples/doc/overview-summary.html samples/your-test-results/sample3.html
	mv samples/doc/SAMPLE.edoc samples/your-test-results/SAMPLE3.edoc	
	
Then check samples/your-test-results/sample1.html - sample3.html and compare with samples/what-you-should-see/sample1.html, sample2.html and  samples/what-you-could-see/sample3.html.

To create the reference samples:

 **FreeBSD**
	etc/make_samples.sh

or do the following to create six samples and save the results into samples/what-you-should-see/ and samples/what-you-could-see/

 **FreeBSD**
	sed -E -f bin/markedoc.sed samples/SAMPLE1.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample1.html
	mv samples/doc/SAMPLE.edoc samples/what-you-should-see/SAMPLE1.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE2.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample2.html
	mv samples/doc/SAMPLE.edoc samples/what-you-should-see/SAMPLE2.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE3.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample3.html
	mv samples/doc/SAMPLE.edoc samples/what-you-should-see/SAMPLE3.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE1.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample1.html
	mv samples/doc/SAMPLE.edoc samples/what-you-could-see/SAMPLE1.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE2.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample2.html
	mv samples/doc/SAMPLE.edoc samples/what-you-could-see/SAMPLE2.edoc
	
	sed -E -f bin/markedoc.sed samples/SAMPLE3.md > samples/doc/SAMPLE.edoc
	erl -noshell -run edoc_run application "'myapp'" '"samples"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
	mv samples/doc/overview-summary.html samples/what-you-could-see/sample3.html
	mv samples/doc/SAMPLE.edoc samples/what-you-could-see/SAMPLE3.edoc		

To test this very README.md, use markdown.lua, credit Niklas Frykholm, <niklas@frykholm.se>:

	lua etc/markdown.lua README.md
	
### HTML Special Signs 
http://www.mountaindragon.com/html/iso.htm


License
-------
This script is free software. It comes without any warranty.

Author
------
H. Diedrich <hd2010@eonblast.com>

History
-------
	
02/03/11 - 0.3 - **rough edges polished:** Linux, FreeBSD, Mac OS X

* added doc for Linux use
* added support for multi-line '[..]: ... "..."' references
* added footnote signs and sepcial chars:
* dagger, double dagger: (+), (++), stars: (*), (**), (***)  
* superscript 1, 2, 3: (*1), (*2), (*3), copyright (C), (R), (TM),  
* guillemots <<, >> and middle dot ::
* added test batches etc/test-bsd.sh and etc/test-linux.sh
* added css sample in samples/what-you-could-see/ 
* added classes for `<li>' list item tags for '[..]:...'-references
* fixed italic and bold merker interference bullet points
* eliminated [..]: part of '[..]:...'-references, flipping "..." to lead
* dev: sample creation batch make_samples.sh added
	
02/02/11 - 0.2 - **basics complete:** FreeBSD / Mac OS X

* added support for === and --- headline format
* fixed cutting off of last lines 
* fixed page-local anchor jumps
* fixed space in javascript links
* eliminated end-space requirement at end of '[..]:...'-style references.
* eliminated need for echoing '@doc' first into edoc output file
* added javascript title tag setting for '[..]:...'-style references.
	
01/31/11 - 0.1 - **first release:** FreeBSD / Mac OS X
	