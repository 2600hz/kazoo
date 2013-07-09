ROOT = ..

MAKEDIRS = rabbitmq_server-*/Makefile \
       lager-*/Makefile \
       ibrowse-*/Makefile \
       */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
		 cp -a rabbitmq_server-3.1.3/plugins-src/rabbitmq-erlang-client rabbitmq_client-3.1.3

clean: ACTION = clean
clean: $(MAKEDIRS)
	   rm -rf rabbitmq_client-*
	   rm -rf rabbitmq-codegen/

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
