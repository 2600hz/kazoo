EXTRA_DIST = core/exmpp.erl						\
	     core/exmpp_sup.erl						\
	     core/exmpp_internals.erl					\
	     core/exmpp_utils.erl					\
	     core/exmpp_xml.erl						\
	     core/exmpp_xmlstream.erl					\
	     core/exmpp_stringprep.erl					\
	     core/exmpp_caps.erl						\
	     core/exmpp_jid.erl						\
	     core/exmpp_stanza.erl					\
	     core/exmpp_stream.erl					\
	     core/exmpp_dialback.erl					\
	     core/exmpp_iq.erl						\
	     core/exmpp_message.erl					\
	     core/exmpp_presence.erl					\
	     core/exmpp_tls.erl						\
	     core/exmpp_compress.erl					\
	     core/exmpp_known_nss.erl.awk				\
	     core/exmpp_known_nss.erl.js				\
	     core/exmpp_known_elems.erl.awk				\
	     core/exmpp_known_elems.erl.js				\
	     core/exmpp_known_attrs.erl.awk				\
	     core/exmpp_known_attrs.erl.js				\
	     network/exmpp_session.erl					\
	     network/exmpp_socket.erl					\
	     network/exmpp_bosh.erl					\
	     network/exmpp_component.erl				\
	     network/exmpp_dns.erl					\
	     network/exmpp_sasl_digest.erl					\
	     client/exmpp_client_binding.erl				\
	     client/exmpp_client_compression.erl			\
	     client/exmpp_client_disco.erl			\
	     client/exmpp_client_legacy_auth.erl			\
	     client/exmpp_client_privacy.erl				\
	     client/exmpp_client_pubsub.erl				\
	     client/exmpp_client_register.erl				\
	     client/exmpp_client_roster.erl				\
	     client/exmpp_client_sasl.erl				\
	     client/exmpp_client_session.erl				\
	     client/exmpp_client_tls.erl				\
	     client/exmpp_client_muc.erl				\
	     server/exmpp_server_binding.erl				\
	     server/exmpp_server_compression.erl			\
	     server/exmpp_server_legacy_auth.erl			\
	     server/exmpp_server_privacy.erl				\
	     server/exmpp_server_sasl.erl				\
	     server/exmpp_server_session.erl				\
	     server/exmpp_server_tls.erl				\
	     compat/ejabberd_socket.erl					\
	     compat/stringprep.erl					\
	     compat/tls.erl						\
	     compat/xml.erl						\
	     compat/xml_stream.erl

KNOWN_NSS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_nss.in
KNOWN_ELEMS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_elems.in
KNOWN_ATTRS_SOURCE = $(top_srcdir)/include/internal/exmpp_known_attrs.in

BUILT_SOURCES = exmpp_known_nss.erl					\
		exmpp_known_elems.erl					\
		exmpp_known_attrs.erl

CLEANFILES = $(BUILT_SOURCES)

# --------------------------------------------------------------------
# Known namespaces/names/attributes headers generation.
# --------------------------------------------------------------------

exmpp_known_nss.erl: $(srcdir)/core/exmpp_known_nss.erl.awk $(KNOWN_NSS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_nss.erl.awk $(KNOWN_NSS_SOURCE) > $@

exmpp_known_elems.erl: $(srcdir)/core/exmpp_known_elems.erl.awk $(KNOWN_ELEMS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_elems.erl.awk $(KNOWN_ELEMS_SOURCE) > $@

exmpp_known_attrs.erl: $(srcdir)/core/exmpp_known_attrs.erl.awk $(KNOWN_ATTRS_SOURCE)
	$(AWK) -f $(srcdir)/core/exmpp_known_attrs.erl.awk $(KNOWN_ATTRS_SOURCE) > $@

all-local:
	@$(ERL) +B -noshell -pa @top_builddir@/ebin			\
		-eval "case make:all() of				\
		         up_to_date -> halt(0);				\
		         Err ->						\
		           io:format(\"~p~n\", [Err]),			\
		           halt(1)					\
		       end"
