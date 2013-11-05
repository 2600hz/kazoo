ROOT = ..

MAKEDIRS = rabbitmq_server-*/Makefile \
       lager-*/Makefile \
       ibrowse-*/Makefile \
       */Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
		cp -a rabbitmq_server-3.2.0/plugins-src/rabbitmq-erlang-client rabbitmq_client-3.2.0
		rm -f rabbitmq_client-*/Makefile

clean: ACTION = clean
clean: $(MAKEDIRS)
	rm -rf rabbitmq_client-*
	rm -rf rabbitmq-codegen/
	rm -rf lager-2.0.0rc1
	rm -rf rabbitmq_server-2.8.6
	rm -rf goldrush-0.1.0

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
