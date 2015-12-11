ROOT = ../..
PROJECT = whistle_apps

EBINS = $(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_caches-* -maxdepth 2 -name ebin -print)

TEST_EBINS = $(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_config-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_media-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
