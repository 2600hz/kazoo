ROOT = ../..
PROJECT = whistle_apps

EBINS = $(wildcard $(ROOT)/core/whistle_apps-*/ebin) \
	$(wildcard $(ROOT)/core/kazoo_caches-*/ebin)

TEST_EBINS = $(wildcard $(ROOT)/core/whistle_number_manager-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_config-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_media-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_couch-*/ebin)

all: compile

-include $(ROOT)/make/kz.mk
