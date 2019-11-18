CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = kazoo_web

# Mimetypes module generator.
GEN_URL = https://raw.githubusercontent.com/apache/httpd/trunk/docs/conf/mime.types
GEN_FILE = mime.types
GEN_ERL_SRC = src/kz_mime.erl.src
GEN_ERL = src/kz_mime.erl

SOURCES = $(wildcard src/*.erl) $(GEN_ERL)

all: compile

compile: $(GEN_ERL)

compile-test: $(GEN_ERL)

include $(ROOT)/make/kz.mk

clean-generated:
	$(if $(wildcard $(GEN_ERL)), @rm $(GEN_ERL))

$(GEN_FILE):
	wget -qO $@ $(GEN_URL)

$(GEN_ERL): $(GEN_FILE) $(GEN_ERL_SRC)
	@head -n `grep -n '%% GENERATED' $(GEN_ERL_SRC) | cut -d: -f1` $(GEN_ERL_SRC) > $(GEN_ERL)
	@echo 'from_extension(Ext) when not is_binary(Ext) -> from_extension(kz_term:to_lower_binary(Ext));' >> $(GEN_ERL)
	@echo 'from_extension(<<$$., Ext/binary>>) -> from_extension(kz_term:to_lower_binary(Ext));' >> $(GEN_ERL)
	@cat $(GEN_FILE) \
		| grep -v ^# \
		| awk '{for (i=2; i<=NF; i++) if ($$i != "") { \
			print "from_extension(<<\"" $$i "\">>) -> <<\"" $$1 "\">>;"}}' \
		| sort -u -t' ' -k1,1 \
		>> $(GEN_ERL)
	@echo 'from_extension(<<"pem">>) -> <<"application/x-pem-file">>;' >> $(GEN_ERL)
	@echo 'from_extension(_) -> <<"application/octet-stream">>.' >> $(GEN_ERL)
	@echo >> $(GEN_ERL)
	@echo '-spec to_extensions(binary()) -> [binary()].' >> $(GEN_ERL)
	@echo 'to_extensions(<<"audio/mp3">>) -> [<<"mp3">>];' >> $(GEN_ERL)
	@echo 'to_extensions(<<"audio/wav">>) -> [<<"wav">>];' >> $(GEN_ERL)
	@echo 'to_extensions(<<"audio/x-wave">>) -> [<<"wav">>];' >> $(GEN_ERL)
	@echo 'to_extensions(<<"application/x-pem-file">>) -> [<<"pem">>];' >> $(GEN_ERL)
	@cat $(GEN_FILE) \
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
		>> $(GEN_ERL)
	@echo 'to_extensions(_) -> [<<>>].' >> $(GEN_ERL)
	@tail -n +`grep -n '%% GENERATED' $(GEN_ERL_SRC) | cut -d: -f1` $(GEN_ERL_SRC) >> $(GEN_ERL)
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(GEN_ERL)
