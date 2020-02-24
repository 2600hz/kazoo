CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = fax

PDF = test/pdf.pdf

all: compile

comile-test: assets

assets: $(PDF)

$(PDF):
	wget -qO $@ https://raw.githubusercontent.com/mathiasbynens/small/master/pdf.pdf

include $(ROOT)/make/kz.mk
