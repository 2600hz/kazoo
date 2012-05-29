ERL_FILES = ibrowse_test_server.erl


INCLUDE_DIRS = -I./ 

ERLC ?= erlc
ERLC_EMULATOR ?= erl -boot start_clean
COMPILER_OPTIONS = -W +warn_unused_vars +nowarn_shadow_vars +warn_unused_import

.SUFFIXES: .erl .beam $(SUFFIXES)

all: $(ERL_FILES:%.erl=%.beam)

%.beam: %.erl
	${ERLC} $(COMPILER_OPTIONS) $(INCLUDE_DIRS) -o ./ $<

clean:
	rm -f *.beam 

