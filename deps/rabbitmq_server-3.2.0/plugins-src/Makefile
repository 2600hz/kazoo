.PHONY: default
default:
	@echo No default target && false

REPOS:= \
    rabbitmq-server \
    rabbitmq-codegen \
    rabbitmq-java-client \
    rabbitmq-dotnet-client \
    rabbitmq-test \
    cowboy-wrapper \
    eldap-wrapper \
    erlang-rfc4627-wrapper \
    mochiweb-wrapper \
    rabbitmq-amqp1.0 \
    rabbitmq-auth-backend-ldap \
    rabbitmq-auth-mechanism-ssl \
    rabbitmq-consistent-hash-exchange \
    rabbitmq-erlang-client \
    rabbitmq-federation \
    rabbitmq-federation-management \
    rabbitmq-jsonrpc \
    rabbitmq-jsonrpc-channel \
    rabbitmq-jsonrpc-channel-examples \
    rabbitmq-management \
    rabbitmq-management-agent \
    rabbitmq-management-visualiser \
    rabbitmq-metronome \
    rabbitmq-web-dispatch \
    rabbitmq-mqtt \
    rabbitmq-shovel \
    rabbitmq-shovel-management \
    rabbitmq-stomp \
    rabbitmq-toke \
    rabbitmq-tracing \
    rabbitmq-web-stomp \
    rabbitmq-web-stomp-examples \
    sockjs-erlang-wrapper \
    toke \
    webmachine-wrapper

BRANCH:=default

HG_CORE_REPOBASE:=$(shell dirname `hg paths default 2>/dev/null` 2>/dev/null)
ifndef HG_CORE_REPOBASE
HG_CORE_REPOBASE:=http://hg.rabbitmq.com/
endif

VERSION:=0.0.0

#----------------------------------

all:
	$(MAKE) -f all-packages.mk all-packages VERSION=$(VERSION)

test:
	$(MAKE) -f all-packages.mk test-all-packages VERSION=$(VERSION)

release:
	$(MAKE) -f all-packages.mk all-releasable VERSION=$(VERSION)

clean:
	$(MAKE) -f all-packages.mk clean-all-packages

check-xref:
	$(MAKE) -f all-packages.mk check-xref-packages

plugins-dist: release
	rm -rf $(PLUGINS_DIST_DIR)
	mkdir -p $(PLUGINS_DIST_DIR)
	$(MAKE) -f all-packages.mk copy-releasable VERSION=$(VERSION) PLUGINS_DIST_DIR=$(PLUGINS_DIST_DIR)

plugins-srcdist:
	rm -rf $(PLUGINS_SRC_DIST_DIR)
	mkdir -p $(PLUGINS_SRC_DIST_DIR)/licensing

	rsync -a --exclude '.hg*' rabbitmq-erlang-client $(PLUGINS_SRC_DIST_DIR)/
	touch $(PLUGINS_SRC_DIST_DIR)/rabbitmq-erlang-client/.srcdist_done

	rsync -a --exclude '.hg*' rabbitmq-server $(PLUGINS_SRC_DIST_DIR)/
	touch $(PLUGINS_SRC_DIST_DIR)/rabbitmq-server/.srcdist_done

	$(MAKE) -f all-packages.mk copy-srcdist VERSION=$(VERSION) PLUGINS_SRC_DIST_DIR=$(PLUGINS_SRC_DIST_DIR)
	cp Makefile *.mk generate* $(PLUGINS_SRC_DIST_DIR)/
	echo "This is the released version of rabbitmq-public-umbrella. \
You can clone the full version with: hg clone http://hg.rabbitmq.com/rabbitmq-public-umbrella" > $(PLUGINS_SRC_DIST_DIR)/README

	PRESERVE_CLONE_DIR=1 make -C $(PLUGINS_SRC_DIST_DIR) clean
	rm -rf $(PLUGINS_SRC_DIST_DIR)/rabbitmq-server

#----------------------------------
# Convenience aliases

.PHONY: co
co: checkout

.PHONY: ci
ci: checkin

.PHONY: up
up: update

.PHONY: st
st: status

.PHONY: up_c
up_c: named_update

#----------------------------------

$(REPOS):
	hg clone $(HG_CORE_REPOBASE)/$@

.PHONY: checkout
checkout: $(REPOS)

#----------------------------------
# Subrepository management


# $(1) is the target
# $(2) is the target dependency. Can use % to get current REPO
# $(3) is the target body. Can use % to get current REPO
define repo_target

.PHONY: $(1)
$(1): $(2)
	$(3)

endef

# $(1) is the list of repos
# $(2) is the suffix
# $(3) is the target dependency. Can use % to get current REPO
# $(4) is the target body. Can use % to get current REPO
define repo_targets
$(foreach REPO,$(1),$(call repo_target,$(REPO)+$(2),\
	$(patsubst %,$(3),$(REPO)),$(patsubst %,$(4),$(REPO))))
endef

# Do not allow status to fork with -j otherwise output will be garbled
.PHONY: status
status: checkout
	$(foreach DIR,. $(REPOS), \
		(cd $(DIR); OUT=$$(hg st -mad); \
		if \[ ! -z "$$OUT" \]; then echo "\n$(DIR):\n$$OUT"; fi) &&) true

.PHONY: pull
pull: $(foreach DIR,. $(REPOS),$(DIR)+pull)

$(eval $(call repo_targets,. $(REPOS),pull,| %,(cd % && hg pull)))

.PHONY: update
update: $(foreach DIR,. $(REPOS),$(DIR)+update)

$(eval $(call repo_targets,. $(REPOS),update,%+pull,(cd % && hg up)))

.PHONY: named_update
named_update: $(foreach DIR,. $(REPOS),$(DIR)+named_update)

$(eval $(call repo_targets,. $(REPOS),named_update,%+pull,\
	(cd % && hg up -C $(BRANCH))))

.PHONY: tag
tag: $(foreach DIR,. $(REPOS),$(DIR)+tag)

$(eval $(call repo_targets,. $(REPOS),tag,| %,(cd % && hg tag $(TAG))))

.PHONY: push
push: $(foreach DIR,. $(REPOS),$(DIR)+push)

# "|| true" sicne hg push fails if there are no changes
$(eval $(call repo_targets,. $(REPOS),push,| %,(cd % && hg push -f || true)))

.PHONY: checkin
checkin: $(foreach DIR,. $(REPOS),$(DIR)+checkin)

$(eval $(call repo_targets,. $(REPOS),checkin,| %,(cd % && hg ci)))
