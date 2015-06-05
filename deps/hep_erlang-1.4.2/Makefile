all: app | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/v2.4.1/erl.mk' || rm $@

-include erl.mk
# Your targets after this line.

## Example:
test: eunit
debug: debug-app
clean: clean-ebin
distclean: clean
	$(if $(wildcard erl.mk), rm erl.mk)

.PHONY: test debug clean distclean

app: ERLCFLAGS += +debug_info
