.PHONY: ci ci-config ci-steps ci-pre ci-fmt ci-build ci-codechecks ci-docs ci-schemas ci-dialyze ci-release ci-unstaged

PIP := $(shell { command -v pip; } 2>/dev/null)
CI_DIR := $(CURDIR)/make
CI_VALIDATOR := $(CI_DIR)/circleci
CI_CONFIG := $(CURDIR)/.circleci/config.yml

ci: ci-config ci-steps

ci-config: $(CI_VALIDATOR)
	@$(CI_VALIDATOR) config validate -c $(CI_CONFIG) || (echo "$(CI_CONFIG):1:"; exit 1)

# | $(CI_DIR): see https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types
# order-only-prereq
# otherwise cURL will be run everytime
$(CI_VALIDATOR): | $(CI_DIR)
	@curl -fLSs https://circle.ci/cli | DESTDIR="$(CI_DIR)" bash

$(CI_DIR):
	@mkdir $(CI_DIR)

ci-steps: ci-pre ci-fmt ci-build ci-codechecks ci-docs ci-schemas ci-dialyze ci-release
	@$(ROOT)/scripts/check-unstaged.bash

ci-pre:
ifneq ($(PIP),)
## needs root access
	@echo $(CHANGED)
	@$(PIP) install --user --upgrade pip
	@$(PIP) install --user PyYAML mkdocs pyembed-markdown jsonschema
else
	$(error "pip is not available, please install python3-pip package")
endif

ci-docs:
	@./scripts/state-of-docs.py || true
	@$(ROOT)/scripts/state-of-edoc.escript
	@$(MAKE) apis
	@$(MAKE) docs

ci-codechecks:
	@./scripts/code_checks.bash $(CHANGED)
	@$(MAKE) code_checks
	@$(MAKE) app_applications
	@$(MAKE) validate-js

ci-fmt:
	@$(MAKE) fmt
	@$(MAKE) elvis

ci-build:
	@$(MAKE) clean clean-deps deps kazoo xref sup_completion

ci-schemas:
	@$(MAKE) validate-schemas
	@$(if $(CHANGED_SWAGGER), $(MAKE) ci-swagger)

ci-swagger:
	@-$(MAKE) validate-swagger

ci-unstaged:
	echo Unstaged changes!
	git status --porcelain
	git --no-pager diff
	echo 'Maybe try `make apis` and see if that fixes anything ;)'
	exit 1

ci-dialyze: build-plt
ci-dialyze:
	@TO_DIALYZE="$(CHANGED)" $(MAKE) dialyze-it

ci-release:
	@$(MAKE) build-ci-release
