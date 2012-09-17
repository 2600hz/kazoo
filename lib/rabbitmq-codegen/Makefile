all: app

app: compile

compile:
	echo "Please select a target from the Makefile."

clean:
	rm -f *.pyc

distclean: clean
	find . -regex '.*\(~\|#\|\.swp\)' -exec rm {} \;
