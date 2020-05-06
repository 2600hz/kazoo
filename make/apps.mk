NO_AUTOPATCH_ERLANG_MK = 1

## If you use SSH keys instead
## FETCH_AS = git@github.com:

FETCH_AS ?= https://github.com/

DEPS ?= blackhole \
	braintree \
	call_inspector \
	callflow \
	cdr \
	conference \
	crossbar \
	doodle \
	ecallmgr \
	fax \
	hangups \
	hotornot \
	jonny5 \
	media_mgr \
	milliwatt \
	omnipresence \
	pivot \
	pusher \
	registrar \
	reorder \
	skel \
	stats \
	stepswitch \
	sysconf \
	tasks \
	teletype \
	trunkstore \
	webhooks

NO_AUTOPATCH = $(DEPS)

dep_blackhole = git $(FETCH_AS)2600hz/kazoo-blackhole.git master
dep_braintree = git $(FETCH_AS)2600hz/kazoo-braintree.git master
dep_call_inspector = git $(FETCH_AS)2600hz/kazoo-call-inspector.git master
dep_callflow = git $(FETCH_AS)2600hz/kazoo-callflow.git master
dep_cdr = git $(FETCH_AS)2600hz/kazoo-cdr.git master
dep_conference = git $(FETCH_AS)2600hz/kazoo-conference.git master
dep_crossbar = git $(FETCH_AS)2600hz/kazoo-crossbar.git master
dep_doodle = git $(FETCH_AS)2600hz/kazoo-doodle.git master
dep_ecallmgr = git $(FETCH_AS)2600hz/kazoo-ecallmgr.git master
dep_fax = git $(FETCH_AS)2600hz/kazoo-fax.git master
dep_hangups = git $(FETCH_AS)2600hz/kazoo-hangups.git master
dep_hotornot = git $(FETCH_AS)2600hz/kazoo-hotornot.git master
dep_jonny5 = git $(FETCH_AS)2600hz/kazoo-jonny5.git master
dep_media_mgr = git $(FETCH_AS)2600hz/kazoo-media-mgr.git master
dep_milliwatt = git $(FETCH_AS)2600hz/kazoo-milliwatt.git master
dep_omnipresence = git $(FETCH_AS)2600hz/kazoo-omnipresence.git master
dep_pivot = git $(FETCH_AS)2600hz/kazoo-pivot.git master
dep_pusher = git $(FETCH_AS)2600hz/kazoo-pusher.git master
dep_registrar = git $(FETCH_AS)2600hz/kazoo-registrar.git master
dep_reorder = git $(FETCH_AS)2600hz/kazoo-reorder.git master
dep_skel = git $(FETCH_AS)2600hz/kazoo-skel.git master
dep_stats = git $(FETCH_AS)2600hz/kazoo-stats.git master
dep_stepswitch = git $(FETCH_AS)2600hz/kazoo-stepswitch.git master
dep_sysconf = git $(FETCH_AS)2600hz/kazoo-sysconf.git master
dep_tasks = git $(FETCH_AS)2600hz/kazoo-tasks.git master
dep_teletype = git $(FETCH_AS)2600hz/kazoo-teletype.git master
dep_trunkstore = git $(FETCH_AS)2600hz/kazoo-trunkstore.git master
dep_webhooks = git $(FETCH_AS)2600hz/kazoo-webhooks.git master

dep_kazoo_ast = git $(FETCH_AS)2600hz/kazoo-ast.git master

-include more_apps.mk
# Add community deps from https://github.com/kazoo-community?utf8=%E2%9C%93&q=kazoo-&type=&language=
# Or add your own private deps to `more_apps.mk` (which is not version-controlled)
# DEPS += {APP1} {APP2} {APP3}

# Then add how, from where, and what branch/tag/commit SHA to use for the dep
# See https://erlang.mk/guide/deps.html section 7.2.2 for fetch options
# dep_{APP1} = git $(FETCH_AS)kazoo-community/kazoo-{APP1}.git master
