FMT_DIR = $(ROOT)/make/erlang-formatter
FMT = $(FMT_DIR)/fmt.sh
FMT_SHA = 3d3fa5b7ce5d144142689850914ea893c5881439
FMT_SHA_FILE = $(FMT_DIR)/.$(FMT_SHA)

$(FMT_SHA_FILE):
	$(info fetching latest formatter)
	@wget -qO - 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/$(FMT_SHA)' | tar -vxz -C $(ROOT)/make/
	@$(if $(wildcard $(FMT_DIR)), rm -r $(FMT_DIR))
	@mv $(ROOT)/make/erlang-formatter-$(FMT_SHA) $(FMT_DIR)
	@touch $(FMT_SHA_FILE)

.PHONY: fmt-all
fmt-all: $(FMT_SHA_FILE)
	@$(FMT) $(shell find core applications scripts -name "*.erl" -or -name "*.hrl" -or -name "*.escript")

.PHONY: fmt
fmt: TO_FMT ?= $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- "*.erl" "*.hrl" "*.escript")
fmt: $(FMT_SHA_FILE)
	@$(if $(TO_FMT), @$(FMT) $(TO_FMT))

.PHONY: fmt-views-all
fmt-views-all:
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find core/kazoo_apps/priv/couchdb/account -name '*.json')
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find applications core -wholename '*/couchdb/views/*.json')

.PHONY: fmt-views
fmt-views: TO_FMT_VIEWS ?= $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- "*/couchdb/views/*.json" "*/couchdb/account/*.json")
fmt-views:
	@$(if $(TO_FMT_VIEWS), @$(ROOT)/scripts/format-couchdb-views.py $(TO_FMT_VIEWS))

.PHONY: clean-fmt
clean-fmt: clean-$(FMT)

clean-$(FMT):
	$(if $(wildcard $(FMT)), rm -r $(FMT_DIR))
