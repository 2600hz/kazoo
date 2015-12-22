ROOT = ..

MAKEDIRS = lager-*/Makefile \
	ibrowse-*/Makefile \
	cowlib-*/Makefile \
	wsock-*/Makefile \
	merl-*/Makefile \
	*/Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
