ROOT = ..

MAKEDIRS = rabbitmq_server-*/Makefile \
	lager-*/Makefile \
	ibrowse-*/Makefile \
	cowlib-*/Makefile \
	wsock-*/Makefile \
	*/Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
	cp -a rabbitmq_server-3.3.5/plugins-src/rabbitmq-erlang-client rabbitmq_client-3.3.5
	rm -f rabbitmq_client-*/Makefile

clean: ACTION = clean
clean: $(MAKEDIRS)
	rm -rf rabbitmq_client-*
	rm -rf rabbitmq_server-3.2.0
	rm -rf goldrush-0.1.0
	rm -rf cowboy-0.9.0
	rm -rf cowlib-0.4.0
	rm -rf ranch-0.9.0

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
