ROOT = ../..
PROJECT = kazoo_auth

all: compile

# Target to be built by `compile` and `compile-test`
COMPILE_MOAR = priv/kz_auth_rsa_drv.so
CLEAN_MOAR = clean-so clean-c_src-env

include $(ROOT)/make/kz.mk

ifeq ($(PLATFORM),darwin)
	CFLAGS = -c -O3 -std=c99 -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -bundle -flat_namespace -undefined suppress

# Apple has deprecated use of OpenSSL in favor of its own TLS and crypto libraries
	OSXRELEASE = $(shell uname -r | sed 's/\..*//')
	ifeq ($(filter $(OSXRELEASE), 15-16),) #El Capitan, Sierra
		OPENSSL_BREW_PREFIX = $(shell brew --prefix openssl)
		CFLAGS += -I$(OPENSSL_BREW_PREFIX)/include
		LDFLAGS += -L$(OPENSSL_BREW_PREFIX)/lib
	endif
else ifeq ($(PLATFORM),freebsd)
	CFLAGS = -c -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -shared
else ifeq ($(PLATFORM),solaris)
	CFLAGS = -c -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -shared
else ifeq ($(PLATFORM),linux)
	CFLAGS = -c -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -shared
endif

c_src/kz_auth_rsa_drv.o: CFLAGS += -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR)
c_src/kz_auth_rsa_drv.o: c_src/kz_auth_rsa_drv.c
	$(CC) $(CFLAGS) c_src/kz_auth_rsa_drv.c -o c_src/kz_auth_rsa_drv.o

priv/kz_auth_rsa_drv.so: LDFLAGS += -lcrypto -L$(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
priv/kz_auth_rsa_drv.so: c_src/kz_auth_rsa_drv.o
	$(CC) c_src/kz_auth_rsa_drv.o $(LDFLAGS) -o priv/kz_auth_rsa_drv.so

clean-so:
	$(if $(wildcard c_src/kz_auth_rsa_drv.o), rm c_src/kz_auth_rsa_drv.o)
	$(if $(wildcard priv/kz_auth_rsa_drv.so), rm priv/kz_auth_rsa_drv.so)

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
