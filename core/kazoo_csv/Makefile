ROOT = ../..
PROJECT = kazoo_csv

all: compile

test: check_csvs

check_csvs:
	@stat -t -- test/*.csv >/dev/null 2>/dev/null ; if [[ $$? > 0 ]] ; then echo WARNING: run 'make csvs' for more test data ; fi

csvs:
	./download_csvs.py

include $(ROOT)/make/kz.mk
