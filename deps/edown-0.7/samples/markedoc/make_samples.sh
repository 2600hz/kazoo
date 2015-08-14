echo "making markedoc samples"

sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE1.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/what-you-should-see/sample1.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/what-you-should-see/SAMPLE1.edoc

sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE2.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/what-you-should-see/sample2.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/what-you-should-see/SAMPLE2.edoc

sed -E -f bin/markedoc.sed samples/markedoc/SAMPLE3.md > samples/markedoc/doc/SAMPLE.edoc
erl -noshell -run edoc_run application "'myapp'" '"samples/markedoc"' '[{def,{vsn,""}},{stylesheet, "markedoc.css"}]'
mv samples/markedoc/doc/overview-summary.html samples/markedoc/what-you-should-see/sample3.html
mv samples/markedoc/doc/SAMPLE.edoc samples/markedoc/what-you-should-see/SAMPLE3.edoc	

echo "done"
echo "see samples/markedoc/what-you-should-see/sample1.html - sample3.html"
