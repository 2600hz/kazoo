ROOT = ..

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

clean-test: ACTION = clean-test
clean-test: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
