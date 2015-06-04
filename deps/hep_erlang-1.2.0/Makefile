all: app | erl.mk

erl.mk:
	curl -fsSLo $@ 'https://raw.github.com/fenollp/erl-mk/master/erl.mk' || rm $@

-include erl.mk
# Your targets after this line.

## Example:
test: eunit
debug: debug-app
clean: clean-ebin

.PHONY: test debug clean
