execdrivers_LTLIBRARIES = exmpp_stringprep.la

if WITH_EXPAT
execdrivers_LTLIBRARIES += exmpp_xml_expat.la exmpp_xml_expat_legacy.la
endif

if WITH_LIBXML2
execdrivers_LTLIBRARIES += exmpp_xml_libxml2.la
endif

if WITH_OPENSSL
execdrivers_LTLIBRARIES += exmpp_tls_openssl.la
endif

if WITH_ZLIB
execdrivers_LTLIBRARIES += exmpp_compress_zlib.la
endif

EXTRA_DIST = stringprep_uni_data.h stringprep_uni_norm.h

EI_CPPFLAGS = -I$(ERLANG_ERTS_DIR)/include				\
	      -I$(ERLANG_LIB_DIR_erl_interface)/include
EI_LDFLAGS = -L$(ERLANG_LIB_DIR_erl_interface)/lib
EI_LIBS = -lei_st

execdriversdir = $(ERLANG_INSTALL_LIB_DIR_exmpp)/priv/lib

# --------------------------------------------------------------------
# Supported port drivers.
# --------------------------------------------------------------------

# Expat port drivers.
exmpp_xml_expat_la_CPPFLAGS = $(SMP_CPPFLAGS)				\
			      $(EI_CPPFLAGS)				\
			      $(EXPAT_CPPFLAGS)
exmpp_xml_expat_la_LDFLAGS = -module -avoid-version			\
			     $(ERL_DRIVER_LDFLAGS)			\
			     $(EI_LDFLAGS) $(EI_LIBS)			\
			     $(EXPAT_LDFLAGS) $(EXPAT_LIBS)
exmpp_xml_expat_la_SOURCES = exmpp_driver.h				\
			     exmpp_driver.c				\
			     exmpp_xml.h				\
			     exmpp_xml.c				\
			     exmpp_xml_expat.c

exmpp_xml_expat_legacy_la_CPPFLAGS = $(SMP_CPPFLAGS)			\
				     $(EI_CPPFLAGS)			\
				     $(EXPAT_CPPFLAGS)
exmpp_xml_expat_legacy_la_LDFLAGS = -module -avoid-version		\
				    $(ERL_DRIVER_LDFLAGS)		\
				    $(EI_LDFLAGS) $(EI_LIBS)		\
				    $(EXPAT_LDFLAGS) $(EXPAT_LIBS)
exmpp_xml_expat_legacy_la_SOURCES = exmpp_driver.h			\
				    exmpp_driver.c			\
				    exmpp_xml.h				\
				    exmpp_xml.c				\
				    exmpp_xml_expat_legacy.c

# LibXML2 port drivers.
exmpp_xml_libxml2_la_CPPFLAGS = $(SMP_CPPFLAGS)				\
				$(EI_CPPFLAGS)
exmpp_xml_libxml2_la_CFLAGS = $(LIBXML2_CFLAGS)
exmpp_xml_libxml2_la_LDFLAGS = -module -avoid-version			\
			       $(ERL_DRIVER_LDFLAGS)			\
			       $(EI_LDFLAGS) $(EI_LIBS)			\
			       $(LIBXML2_LIBS)
exmpp_xml_libxml2_la_SOURCES = exmpp_driver.h				\
			       exmpp_driver.c				\
			       exmpp_xml.h				\
			       exmpp_xml.c				\
			       exmpp_xml_libxml2.c

# Stringprep port driver.
exmpp_stringprep_la_CPPFLAGS = $(EI_CPPFLAGS)
exmpp_stringprep_la_LDFLAGS = -module -avoid-version			\
			      $(ERL_DRIVER_LDFLAGS)			\
			      $(EI_LDFLAGS) $(EI_LIBS)
exmpp_stringprep_la_SOURCES = exmpp_driver.h				\
			      exmpp_driver.c				\
			      stringprep_uni_data.h			\
			      stringprep_uni_norm.h			\
			      exmpp_stringprep.c

# OpenSSL TLS port driver.
exmpp_tls_openssl_la_CPPFLAGS = $(EI_CPPFLAGS)				\
				$(OPENSSL_CPPFLAGS)
exmpp_tls_openssl_la_LDFLAGS = -module -avoid-version			\
			       $(ERL_DRIVER_LDFLAGS)			\
			       $(EI_LDFLAGS) $(EI_LIBS)			\
			       $(OPENSSL_LDFLAGS) $(OPENSSL_LIBS)
exmpp_tls_openssl_la_SOURCES = exmpp_driver.h				\
			       exmpp_driver.c				\
			       exmpp_tls.h				\
			       exmpp_tls.c				\
			       exmpp_tls_openssl.c

# Zlib compression port driver.
exmpp_compress_zlib_la_CPPFLAGS = $(EI_CPPFLAGS)			\
				  $(ZLIB_CPPFLAGS)
exmpp_compress_zlib_la_LDFLAGS = -module -avoid-version			\
				 $(ERL_DRIVER_LDFLAGS)			\
				 $(EI_LDFLAGS) $(EI_LIBS)		\
				 $(ZLIB_LDFLAGS) $(ZLIB_LIBS)
exmpp_compress_zlib_la_SOURCES = exmpp_driver.h				\
				 exmpp_driver.c				\
				 exmpp_compress.h			\
				 exmpp_compress_zlib.c
