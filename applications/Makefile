ROOT = ../

MAKEDIRS = */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

test: ACTION = test
test: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
