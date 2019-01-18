FMT = $(ROOT)/make/erlang-formatter/fmt.sh
FMT_SHA = 237604a566879bda46d55d9e74e3e66daf1b557a

.PHONY: fmt fmt-all clean-fmt clean-$(FMT)

$(FMT):
	@wget -qO - 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/$(FMT_SHA)' | tar -vxz -C $(ROOT)/make/
	@mv $(ROOT)/make/erlang-formatter-$(FMT_SHA) $(ROOT)/make/erlang-formatter
	@cp $(ROOT)/make/emacs/*.el $(ROOT)/make/erlang-formatter/emacs

fmt-all: $(FMT)
	@$(FMT) $(shell find core applications scripts -name "*.erl" -or -name "*.hrl" -or -name "*.escript")

fmt: TO_FMT ?= $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- "*.erl" "*.hrl" "*.escript")
fmt: $(FMT)
	@$(if $(TO_FMT), @$(FMT) $(TO_FMT))

clean-fmt: clean-$(FMT)

clean: clean-$(FMT)

clean-$(FMT):
	$(if $(wildcard $(FMT)), rm -r $(dir $(FMT)))
