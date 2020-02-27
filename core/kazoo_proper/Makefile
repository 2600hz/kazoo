CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_proper

ERLC_OPTS = +'{lager_extra_sinks, [data]}'

MP3 = priv/mp3.mp3

COMPILE_MOAR = assets

all: compile assets

assets: $(MP3)

$(MP3):
	@mkdir -p $(shell dirname $(MP3))
	wget -qO $@ https://raw.githubusercontent.com/mathiasbynens/small/master/mp3.mp3

include $(ROOT)/make/kz.mk
