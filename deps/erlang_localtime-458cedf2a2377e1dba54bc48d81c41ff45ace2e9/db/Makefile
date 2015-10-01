TZDIR=tzdata
TZ_FILES=$(addprefix $(TZDIR)/, africa antarctica asia australasia backward etcetera europe northamerica southamerica)

tzout: $(TZ_FILES)
	TZ_VERSION=`perl -n -e 'm/^VERSION\s*=\s*(\S+)/ and print $$1;' $(TZDIR)/Makefile`; \
	./tz-erl --version $$TZ_VERSION -o $@ $^

$(TZ_FILES): $(TZDIR)

$(TZDIR): tzdata-latest.tar.gz
	mkdir $(TZDIR) && cd $(TZDIR) && tar xzf ../$<

tzdata-latest.tar.gz:
	curl -O "ftp://ftp.iana.org/tz/tzdata-latest.tar.gz"


