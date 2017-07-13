.PHONY: docs-build docs-serve clean

DOCS_ROOT ?= $(shell readlink -f .)

YML ?= $(filter-out local,$(wildcard *.yml))
LOCAL = $(YML:.yml=.local)

clean:
	@rm -rf $(DOCS_ROOT)/site $(DOCS_ROOT)/docs $(LOCAL)

docs-build: $(LOCAL)

%.local: %.yml
	@$(shell cp $< $@)
	@$(shell cp "$*.md" $(DOCS_ROOT)/docs/index.md)
	@if [ -f $(DOCS_ROOT)/theme/global.yml ]; then cat $(DOCS_ROOT)/theme/global.yml >> $@ ; fi
	@echo "\ntheme: null\ntheme_dir: '$(DOCS_ROOT)/theme'\ndocs_dir: '$(DOCS_ROOT)/docs'\n" >> $@
	@echo "building $*"
	@mkdocs build -f $@ --clean -q --site-dir "$(DOCS_ROOT)/site/$<"

docs-serve: $(LOCAL)
	@mkdocs serve --dev-addr=0.0.0.0:9876 -f $(DOCS_ROOT)/$(LOCAL)
