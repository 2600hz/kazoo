CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = fax

PDF = test/pdf.pdf

COMPILE_MOAR = assets

all: compile assets

comile-test: assets

comile-test-direct: assets

assets: $(PDF)

$(PDF):
	wget -qO $@ https://raw.githubusercontent.com/mathiasbynens/small/master/pdf.pdf

include $(ROOT)/make/kz.mk
