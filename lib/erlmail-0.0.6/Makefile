ERL=erl
APP_NAME=erlmail
NODE_NAME=erlmail
VSN=0.0.6

all: ebin/smtpd.app ebin/imapd.app ebin/smtpc.app $(wildcard src/*.erl)
	$(ERL) -pa lib/*/ebin -I lib/*/include -make

ebin/%.app: src/%.app.src
	cp $< $@

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

run:
	$(ERL) -pa `pwd`/ebin \
	-boot start_sasl \
	-sname $(NODE_NAME)