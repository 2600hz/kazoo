.SUFFIXES:  .beam .hrl .erl .edoc .html .report .pdf


MODS = escript 
ERL_FILES = ${MODS:%=%.erl}
BEAM_FILES = ${MODS:%=%.beam}
ERLC = erlc
VSN = escript-4.1
PWD = `pwd`

.erl.beam: 
	${ERLC} $<

all: ${BEAM_FILES} escript

escript:
	./mk_escript.sh

dist:
	make clean
	mkdir ${VSN}
	cp ${ERL_FILES} Makefile Installing history escript.html mk_escript.sh\
		        hello factorial fibi fibc ${VSN}
	gtar -zcvf ${VSN}.tgz ${VSN}
	rm -rf ${VSN}	

publish: 
	make dist
	cp escript.html ${VSN}.tgz ${HOME}/public_html/external

clean:
	rm -rf *.beam escript ${VSN}.tgz
