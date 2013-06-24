echo -n "testing markedoc - FreeBSD / Mac OS X:  "

echo -n "1 ... "
sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE1.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[{def,{vsn,""}}]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/your-test-results/sample1.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/your-test-results/SAMPLE1.edoc	

echo -n "2 ... "
sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE2.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/your-test-results/sample2.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/your-test-results/SAMPLE2.edoc

echo -n "3 ... "
sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE3.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/your-test-results/sample3.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/your-test-results/SAMPLE3.edoc	

echo "done."
echo "=> now check samples/markedoc/your-test-results/sample1.html - sample3.html"
echo "compare with samples/markedoc/what-you-should-see/sample1.html - sample2.html"
