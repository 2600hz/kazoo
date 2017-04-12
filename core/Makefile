ROOT = ..

MAKEDIRS = */Makefile

.PHONY: all compile compile-test clean clean-test eunit test first $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

compile-test: ACTION = compile-test
compile-test: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

clean-test: ACTION = clean-test
clean-test: $(MAKEDIRS)

eunit: ACTION = eunit
eunit: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

first:
	$(MAKE) -C kazoo_stdlib/ $(ACTION)
	$(MAKE) -C kazoo/ $(ACTION)
	$(MAKE) -C kazoo_amqp/ $(ACTION)
	$(MAKE) -C kazoo_data/ $(ACTION)

$(MAKEDIRS): first
	$(MAKE) -C $(@D)    $(ACTION)
