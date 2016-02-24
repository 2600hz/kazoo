DEPS_DIR = $(CURDIR)
ERLC_OPTS += +debug_info

clean::
	$(if $(wildcard ./*/), rm -rf ./*/)
	$(if $(wildcard .erlang.mk), rm -rf .erlang.mk)


include ../deps.mk
-include ../erlang.mk
