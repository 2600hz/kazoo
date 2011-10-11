var fso, fd, line;
var re, re_srcdir, re_builddir, re_top_srcdir, re_top_builddir;
var re_compat_start, re_compat, re_emkopts;

if (WScript.Arguments.length != 2) {
	WScript.Echo("Syntax: cscript " +
	    "Emakefile.in.js <Emakefile.in> <EMKOPTS>");
} else {
	/* Open the input file for reading (1). */
	fso = new ActiveXObject("Scripting.FileSystemObject");
	fd = fso.OpenTextFile(WScript.Arguments.Item(0), 1, false);

	/* Regexps used to replace macros. */
	re_srcdir = /@srcdir@\//;
	re_builddir = /@builddir@\//;
	re_top_srcdir = /@top_srcdir@/;
	re_top_builddir = /@top_builddir@/;
	re_compat_start = /@COMPAT_MODULES_START@/;
	re_compat = /@COMPAT_MODULES@/;
	re_emkopts = /@EMKOPTS@/;

	while (!fd.AtEndOfStream) {
		line = fd.ReadLine();

		line = line.replace(re_srcdir, "");
		line = line.replace(re_builddir, "");
		line = line.replace(re_top_srcdir, "..");
		line = line.replace(re_top_builddir, "..");
		line = line.replace(re_compat_start, ",");
		line = line.replace(re_compat, "");
		line = line.replace(re_emkopts, WScript.Arguments.Item(1));

		WScript.Echo(line);
	}

	/* Done! */
	fd.close();
}
