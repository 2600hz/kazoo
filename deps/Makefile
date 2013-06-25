ROOT = ..

MAKEDIRS = rabbitmq_server-2.8.6/Makefile \
       rabbitmq_client-2.8.6/Makefile \
       lager-2.0.0rc1/Makefile \
       ibrowse-4.0.1/Makefile \
       */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

clean: ACTION = clean
clean: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
