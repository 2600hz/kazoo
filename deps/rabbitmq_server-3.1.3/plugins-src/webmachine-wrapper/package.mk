APP_NAME:=webmachine
DEPS:=mochiweb-wrapper

UPSTREAM_GIT:=https://github.com/rabbitmq/webmachine.git
UPSTREAM_REVISION:=52e62bc655d700dfc8f409d44db0a07b39c3234f
RETAIN_ORIGINAL_VERSION:=true

WRAPPER_PATCHES:=10-remove-crypto-dependency.patch \
                 20-parameterised-modules-r16a.patch \
                 30-silent-at-startup.patch

ORIGINAL_APP_FILE=$(CLONE_DIR)/ebin/$(APP_NAME).app
DO_NOT_GENERATE_APP_FILE=true

define package_rules

# This rule is run *before* the one in do_package.mk
$(PLUGINS_SRC_DIST_DIR)/$(PACKAGE_DIR)/.srcdist_done::
	cp $(CLONE_DIR)/LICENSE $(PACKAGE_DIR)/LICENSE-Apache-Basho

endef
