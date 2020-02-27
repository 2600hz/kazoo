PEST_SHA = 90609d16a45c558beeb67c16cc68bf630843c2e1
PEST = $(ROOT)/make/pest-$(PEST_SHA)/pest.erl

# See https://github.com/okeuday/pest#usage

.PHONY: pest pest-all

$(PEST):
	wget -qO - 'https://codeload.github.com/okeuday/pest/tar.gz/$(PEST_SHA)' | tar -vxz -C $(ROOT)/make/
	chmod 755 $(PEST)

ifeq (,$(CHANGED_ERL))
pest:
	@echo No Erlang files changed
else
pest: $(PEST)
	$(PEST) -e $(CHANGED_ERL)
endif

pest-all: $(PEST)
	$(PEST) -v -b deps core applications
