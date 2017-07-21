ROOT = ..

BEGIN = erl +A0 -noinput -boot start_clean -eval
END = -s init stop

ERTS_INCLUDE_DIR = $(shell $(BEGIN) 'io:format("~s/erts-~s/include", [code:root_dir(), erlang:system_info(version)]).' $(END))
ERL_INTERFACE_INCLUDE_DIR = $(shell $(BEGIN) 'io:format(code:lib_dir(erl_interface, include)).' $(END))
ERL_INTERFACE_LIB_DIR = $(shell $(BEGIN) 'io:format(code:lib_dir(erl_interface, lib)).' $(END))

all: $(ROOT)/priv/comp128.so

ifeq ($(PLATFORM),)
    UNAME_S := $(shell uname -s)

    ifeq ($(UNAME_S),Linux)
        PLATFORM = linux
    else ifeq ($(UNAME_S),Darwin)
        PLATFORM = darwin
    else ifeq ($(UNAME_S),SunOS)
        PLATFORM = solaris
    else ifeq ($(UNAME_S),GNU)
        PLATFORM = gnu
    else ifeq ($(UNAME_S),FreeBSD)
        PLATFORM = freebsd
    else ifeq ($(UNAME_S),NetBSD)
        PLATFORM = netbsd
    else ifeq ($(UNAME_S),OpenBSD)
        PLATFORM = openbsd
    else ifeq ($(UNAME_S),DragonFly)
        PLATFORM = dragonfly
    else
        $(error Unable to detect platform.)
    endif

    export PLATFORM
endif

ifeq ($(PLATFORM),darwin)
	CFLAGS = -c -O3 -std=c99 -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -bundle -flat_namespace -undefined suppress
else
	CFLAGS = -c -O3 -std=c99 -finline-functions -fstack-protector -Wall -Wmissing-prototypes -fPIC
	LDFLAGS = -shared
endif

comp128.o: CFLAGS += -I$(ERTS_INCLUDE_DIR) -I$(ERL_INTERFACE_INCLUDE_DIR)
comp128.o: comp128.c
comp128.o:
	$(CC) $(CFLAGS) comp128.c -o comp128.o

$(ROOT)/priv/comp128.so: LDFLAGS += -L$(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei
$(ROOT)/priv/comp128.so: comp128.o
	$(CC) comp128.o $(LDFLAGS) -o $(ROOT)/priv/comp128.so

clean:
	$(if $(wildcard comp128.o), rm comp128.o)
	$(if $(wildcard $(ROOT)/priv/comp128.so), rm $(ROOT)/priv/comp128.so)
