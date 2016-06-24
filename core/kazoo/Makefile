ROOT = ../..
PROJECT = kazoo

SOURCES = src/kazoo_transform.erl src/*.erl src/api/*.erl

CLEAN_MOAR = clean-generated

# Mimetypes module generator.

GEN_URL = http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types
GEN_FILE = mime.types
GEN_SRC = src/kz_mime.erl.src
GEN_OUT = src/kz_mime.erl

all: compile | $(GEN_OUT)

include $(ROOT)/make/kz.mk

clean-generated:
	$(if $(wildcard $(GEN_OUT)), rm $(GEN_OUT))

$(GEN_FILE):
	wget -qO $@ $(GEN_URL)

$(GEN_OUT): $(GEN_FILE) $(GEN_SRC)
	head $(GEN_SRC) -n `grep -n '%% GENERATED' $(GEN_SRC) | cut -d: -f1` > $(GEN_OUT)
	echo 'from_extension(Ext) when not is_binary(Ext) -> from_extension(kz_util:to_binary(Ext));' >> $(GEN_OUT)
	echo 'from_extension(<<$$., Ext/binary>>) -> from_extension(Ext);' >> $(GEN_OUT)
	cat $(GEN_FILE) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			print "from_extension(<<\"" $$i "\">>) -> <<\"" $$1 "\">>;"}}' \
		| sort \
		| uniq -w 25 \
		>> $(GEN_OUT)
	echo 'from_extension(_) -> <<"application/octet-stream">>.' >> $(GEN_OUT)
	echo >> $(GEN_OUT)
	echo 'to_extensions(<<"audio/mp3">>) -> [<<"mp3">>];' >> $(GEN_OUT)
	echo 'to_extensions(<<"audio/wav">>) -> [<<"wav">>];' >> $(GEN_OUT)
	cat $(GEN_FILE) \
		| grep -v ^# \
		| awk '{ \
			printf("to_extensions(<<\"%s\">>) -> [", $$1); \
			if ($$1 == "audio/mpeg") { \
				printf("<<\"mp3\">>") \
			} \
			for (i=2; i<=NF; i++) \
				if ($$i != "" && $$i != "mp3") { \
					if (i >= 3 || (i >= 2 && $$1 == "audio/mpeg") ){printf(",")} \
					printf("<<\"%s\">>",  $$i) \
				} \
			print "];" \
			}' \
		| sort \
		>> $(GEN_OUT)
	echo 'to_extensions(_) -> [<<>>].' >> $(GEN_OUT)
	tail $(GEN_SRC) -n +`grep -n '%% GENERATED' $(GEN_SRC) | cut -d: -f1` >> $(GEN_OUT)
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(GEN_OUT)
