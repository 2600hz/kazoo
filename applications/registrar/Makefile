ROOT = ../..
PROJECT = registrar

all: compile

# Target to be built by `compile` and `compile-test`
COMPILE_MOAR = priv/comp128.so
CLEAN_MOAR = clean-so clean-c_src-env

include $(ROOT)/make/kz.mk

ifeq ($(PLATFORM),darwin)
	CFLAGS := -O3 -std=c99 -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS := -bundle -flat_namespace -undefined suppress
else ifeq ($(PLATFORM),freebsd)
	CFLAGS := -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC -shared
else ifeq ($(PLATFORM),linux)
	CFLAGS := -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC -shared
endif

priv/comp128.so: CFLAGS += -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR)
priv/comp128.so: LDFLAGS += -L$(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
priv/comp128.so: c_src/comp128.c
	$(CC) $(CFLAGS) $(LDFLAGS) c_src/comp128.c -o priv/comp128.so

clean-so:
	$(if $(wildcard priv/comp128.so), rm priv/comp128.so)

c_src/env.mk:
	erl +A0 -noinput -boot start_clean -eval "file:write_file(\"c_src/env.mk\", \
		io_lib:format( \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib)])), \
		halt()."


clean-c_src-env:
	rm -f c_src/env.mk

-include c_src/env.mk
