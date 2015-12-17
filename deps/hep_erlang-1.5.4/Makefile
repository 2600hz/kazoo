all: app | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/v2.4.1/erl.mk' || rm $@

-include erl.mk
# Your targets after this line.

## Example:
test: eunit
debug: debug-app
clean: clean-ebin

.PHONY: test debug clean

app: ERLCFLAGS += +debug_info

distclean: clean
	rm erl.mk
