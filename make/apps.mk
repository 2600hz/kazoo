DEPS = blackhole \
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

# HTTPS for typical clones with no dev work
# sed -i 's!git@github.com:!https://github.com/!g'
# git when yo uhave public keys setup
# sed -i 's!https://gituhb.com/!git@github.com:!g'

dep_blackhole = git https://github.com/2600hz/kazoo-blackhole.git master
dep_braintree = git https://github.com/2600hz/kazoo-braintree.git master
dep_call_inspector = git https://github.com/2600hz/kazoo-call_inspector.git master
dep_callflow = git https://github.com/2600hz/kazoo-callflow.git master
dep_cdr = git https://github.com/2600hz/kazoo-cdr.git master
dep_conference = git https://github.com/2600hz/kazoo-conference.git master
dep_crossbar = git https://github.com/2600hz/kazoo-crossbar.git master
dep_doodle = git https://github.com/2600hz/kazoo-doodle.git master
dep_ecallmgr = git https://github.com/2600hz/kazoo-ecallmgr.git master
dep_fax = git https://github.com/2600hz/kazoo-fax.git master
dep_hangups = git https://github.com/2600hz/kazoo-hangups.git master
dep_hotornot = git https://github.com/2600hz/kazoo-hotornot.git master
dep_jonny5 = git https://github.com/2600hz/kazoo-jonny5.git master
dep_media_mgr = git https://github.com/2600hz/kazoo-media_mgr.git master
dep_milliwatt = git https://github.com/2600hz/kazoo-milliwatt.git master
dep_omnipresence = git https://github.com/2600hz/kazoo-omnipresence.git master
dep_pivot = git https://github.com/2600hz/kazoo-pivot.git master
dep_pusher = git https://github.com/2600hz/kazoo-pusher.git master
dep_registrar = git https://github.com/2600hz/kazoo-registrar.git master
dep_reorder = git https://github.com/2600hz/kazoo-reorder.git master
dep_skel = git https://github.com/2600hz/kazoo-skel.git master
dep_stats = git https://github.com/2600hz/kazoo-stats.git master
dep_stepswitch = git https://github.com/2600hz/kazoo-stepswitch.git master
dep_sysconf = git https://github.com/2600hz/kazoo-sysconf.git master
dep_tasks = git https://github.com/2600hz/kazoo-tasks.git master
dep_teletype = git https://github.com/2600hz/kazoo-teletype.git master
dep_trunkstore = git https://github.com/2600hz/kazoo-trunkstore.git master
dep_webhooks = git https://github.com/2600hz/kazoo-webhooks.git master

-include more_apps.mk
# Add community deps from https://github.com/kazoo-community?utf8=%E2%9C%93&q=kazoo-&type=&language=
# Or add your own private deps to `more_apps.mk` (which is not version-controlled)
# DEPS += {APP1} {APP2} {APP3}

# Then add how, from where, and what branch/tag/commit SHA to use for the dep
# See https://erlang.mk/guide/deps.html section 7.2.2 for fetch options
# dep_{APP1} = git https://github.com/kazoo-community/kazoo-{APP1}.git master
