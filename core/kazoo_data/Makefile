ROOT = ../..
PROJECT = kazoo_data

SOURCES = src/kz_data.erl src/*.erl

ERLC_OPTS += +'{parse_transform, kz_perf_transform}' +'{lager_extra_sinks, [data]}'

all: compile

-include $(ROOT)/make/kz.mk
