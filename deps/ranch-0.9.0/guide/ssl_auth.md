SSL client authentication
=========================

Purpose
-------

SSL client authentication is a mechanism allowing applications to
identify certificates. This allows your application to make sure that
the client is an authorized certificate, but makes no claim about
whether the user can be trusted. This can be combined with a password
based authentication to attain greater security.

The server only needs to retain the certificate serial number and
the certificate issuer to authenticate the certificate. Together,
they can be used to uniquely identify a certicate.

As Ranch allows the same protocol code to be used for both SSL and
non-SSL transports, you need to make sure you are in an SSL context
before attempting to perform an SSL client authentication. This
can be done by checking the return value of `Transport:name/0`.

Obtaining client certificates
-----------------------------

You can obtain client certificates from various sources. You can
generate them yourself, or you can use a service like CAcert.org
which allows you to generate client and server certificates for
free.

Following are the steps you need to take to create a CAcert.org
account, generate a certificate and install it in your favorite
browser.

 *  Open [CAcert.org](http://cacert.org) in your favorite browser
 *  Root Certificate link: install both certificates
 *  Join (Register an account)
 *  Verify your account (check your email inbox!)
 *  Log in
 *  Client Certificates: New
 *  Follow instructions to create the certificate
 *  Install the certificate in your browser

You can optionally save the certificate for later use, for example
to extract the `IssuerID` information as will be detailed later on.

Transport configuration
-----------------------

The SSL transport does not request a client certificate by default.
You need to specify the `{verify, verify_peer}` option when starting
the listener to enable this behavior.

``` erlang
{ok, _} = ranch:start_listener(my_ssl, 100,
	ranch_ssl, [
		{port, SSLPort},
		{certfile, PathToCertfile},
		{cacertfile, PathToCACertfile},
		{verify, verify_peer}
	],
	my_protocol, []
).
```

In this example we set the required `port` and `certfile`, but also
the `cacertfile` containing the CACert.org root certificate, and
the option to request the client certificate.

If you enable the `{verify, verify_peer}` option and the client does
not have a client certificate configured for your domain, then no
certificate will be sent. This allows you to use SSL for more than
just authenticated clients.

Authentication
--------------

To authenticate users, you must first save the certificate information
required. If you have your users' certificate files, you can simply
load the certificate and retrieve the information directly.

``` erlang
certfile_to_issuer_id(Filename) ->
	{ok, Data} = file:read_file(Filename),
	[{'Certificate', Cert, not_encrypted}] = public_key:pem_decode(Data),
	{ok, IssuerID} = public_key:pkix_issuer_id(Cert, self),
	IssuerID.
```

The `IssuerID` variable contains both the certificate serial number
and the certificate issuer stored in a tuple, so this value alone can
be used to uniquely identify the user certificate. You can save this
value in a database, a configuration file or any other place where an
Erlang term can be stored and retrieved.

To retrieve the `IssuerID` from a running connection, you need to first
retrieve the client certificate and then extract this information from
it. Ranch does not provide a function to retrieve the client certificate.
Instead you can use the `ssl:peercert/1` function. Once you have the
certificate, you can again use the `public_key:pkix_issuer_id/2` to
extract the `IssuerID` value.

The following function returns the `IssuerID` or `false` if no client
certificate was found. This snippet is intended to be used from your
protocol code.

``` erlang
socket_to_issuer_id(Socket) ->
	case ssl:peercert(Socket) of
		{error, no_peercert} ->
			false;
		{ok, Cert} ->
			{ok, IssuerID} = public_key:pkix_issuer_id(Cert, self),
			IssuerID
	end.
```

You then only need to match the `IssuerID` value to authenticate the
user.
