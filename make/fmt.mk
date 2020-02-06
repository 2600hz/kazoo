FMT = $(ROOT)/make/erlang-formatter/fmt.sh
FMT_SHA = 4e9b3379952e0cb3319308d7bdef832eb305f816

.PHONY: fmt fmt-all fmt-views fmt-views-all clean-fmt clean-$(FMT)

$(FMT):
	@wget -qO - 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/$(FMT_SHA)' | tar -vxz -C $(ROOT)/make/
	@mv $(ROOT)/make/erlang-formatter-$(FMT_SHA) $(ROOT)/make/erlang-formatter

fmt-all: $(FMT)
	@$(FMT) $(shell find core applications scripts -name "*.erl" -or -name "*.hrl" -or -name "*.escript")

fmt: TO_FMT ?= $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- "*.erl" "*.hrl" "*.escript")
fmt: $(FMT)
	@$(if $(TO_FMT), @$(FMT) $(TO_FMT))

fmt-views-all:
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find core/kazoo_apps/priv/couchdb/account -name '*.json')
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find applications core -wholename '*/couchdb/views/*.json')

fmt-views: TO_FMT_VIEWS ?= $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- "*/couchdb/views/*.json" "*/couchdb/account/*.json")
fmt-views:
	@$(if $(TO_FMT_VIEWS), @$(ROOT)/scripts/format-couchdb-views.py $(TO_FMT_VIEWS))

clean-fmt: clean-$(FMT)

clean: clean-$(FMT)

clean-$(FMT):
	$(if $(wildcard $(FMT)), rm -r $(dir $(FMT)))
