ROOT = ..

MAKEDIRS = rabbitmq_server-*/Makefile \
	lager-*/Makefile \
	ibrowse-*/Makefile \
	cowlib-*/Makefile \
	wsock-*/Makefile \
	merl-*/Makefile \
	*/Makefile

.PHONY: all compile clean $(MAKEDIRS)

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)
	cp -a rabbitmq_server-3.5.5/plugins-src/rabbitmq-erlang-client rabbitmq_client-3.5.5
	cp -a rabbitmq_client-3.5.5/deps/rabbit_common-3.5.5 rabbit_common-3.5.5
	rm -f rabbitmq_client-*/Makefile

clean: ACTION = clean
clean: $(MAKEDIRS)
	rm -rf rabbitmq_client-*
	rm -rf rabbit_common-*
	rm -rf rabbitmq_server-3.2.0
	rm -rf goldrush-0.1.0
	rm -rf cowboy-0.9.0
	rm -rf cowlib-0.4.0
	rm -rf ranch-0.9.0
	rm -rf erlang-localtime-1.0.0
	rm -rf gen_smtp-0.0.1
	rm -rf erlydtl-0.9.0
	rm -rf jesse-1.0.1
	rm -rf nksip-0.4

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)
