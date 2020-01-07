# Setup SSL support for Crossbar

Note: all commands are run from `$KAZOO/applications/crossbar/priv/ssl/`.

1. Create the root key (skip if you have a root key already that you want to use):

```shell
$ openssl genrsa -out 2600HzCA.key 2048
Generating RSA private key, 2048 bit long modulus
.............+++
......................................................+++
e is 65537 (0x10001)
```

2. Sign the root key (fill out a questionnaire):

```shell
$ openssl req -x509 -new -nodes -key 2600HzCA.key -days 1024 -out 2600HzCA.pem
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:US
State or Province Name (full name) [Some-State]:California
Locality Name (eg, city) []:San Francisco
Organization Name (eg, company) [Internet Widgits Pty Ltd]:2600Hz
Organizational Unit Name (eg, section) []:
Common Name (e.g. server FQDN or YOUR name) []:api.2600hz.com
Email Address []:
```

3. Create a certificate (cert):

```shell
$ openssl genrsa -out crossbar.key 2048
Generating RSA private key, 2048 bit long modulus
.......+++
......+++
e is 65537 (0x10001)
```

4. Remove the need for a passphrase:

```shell
$ openssl rsa -in crossbar.key -out crossbar.pem
writing RSA key
```

5. Now generate the certificate signing request (CSR):

Note: be sure, when answering the "Common Name" question to put either your FQDN or IP address that will show in the browser.

```shell
$ openssl req -new -key crossbar.key -out crossbar.csr
You are about to be asked to enter information that will be incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [AU]:US
State or Province Name (full name) [Some-State]:California
Locality Name (eg, city) []:San Francisco
Organization Name (eg, company) [Internet Widgits Pty Ltd]:2600Hz
Organizational Unit Name (eg, section) []:
Common Name (e.g. server FQDN or YOUR name) []:api.2600hz.com
Email Address []:

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:
An optional company name []:
```

6. Now let's sign the CSR:

```shell
$ openssl x509 -req -in crossbar.csr -CA 2600HzCA.pem -CAkey 2600HzCA.key -CAcreateserial -out crossbar.crt -days 500
Signature ok
subject=/C=US/ST=California/L=San Francisco/O=2600Hz/CN=thinky64.2600hz.com
Getting CA Private Key
```

7. And finally, generate the self-signed certificate:

```shell
$ openssl x509 -req -days 60 -in crossbar.csr -signkey crossbar.key -out crossbar.crt
Signature ok
subject=/C=US/ST=California/L=San Francisco/O=2600Hz/CN=thinky64.2600hz.com
Getting Private key
```

8. Now modify the `crossbar` doc in the `system_config` database with the following values:

```json
        "default": {
          "use_ssl": true,
          "ssl_port": 8443,
          "ssl_cert": "priv/ssl/crossbar.crt",
          "ssl_key": "priv/ssl/crossbar.key"
        }
```

9. Start Crossbar.

You can now test your new SSL-enabled APIs via:

```shell
$ curl -v --cacert crossbar.crt https://api.2600hz.com:8443/v2/accounts
* About to connect() to api.2600hz.com port 8443 (#0)
*   Trying 127.0.0.1... connected
* successfully set certificate verify locations:
*   CAfile: crossbar.crt
CApath: /etc/ssl/certs
* SSLv3, TLS handshake, Client hello (1):
* SSLv3, TLS handshake, Server hello (2):
* SSLv3, TLS handshake, CERT (11):
* SSLv3, TLS handshake, Server key exchange (12):
* SSLv3, TLS handshake, Server finished (14):
* SSLv3, TLS handshake, Client key exchange (16):
* SSLv3, TLS change cipher, Client hello (1):
* SSLv3, TLS handshake, Finished (20):
* SSLv3, TLS change cipher, Client hello (1):
* SSLv3, TLS handshake, Finished (20):
* SSL connection using DHE-RSA-AES256-SHA
* Server certificate:
*        subject: C=US; ST=California; L=San Francisco; O=2600Hz; CN=api.2600hz.com
*        start date: 2012-06-01 21:59:03 GMT
*        expire date: 2012-07-31 21:59:03 GMT
*        common name: api.2600hz.com (matched)
*        issuer: C=US; ST=California; L=San Francisco; O=2600Hz; CN=api.2600hz.com
*        SSL certificate verify ok.
> GET /v2/accounts HTTP/1.1
> User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3
> Host: api.2600hz.com:8443
> Accept: */*
>
< HTTP/1.1 401 Unauthorized
< Www-Authenticate:
< Access-Control-Max-Age: 86400
< Access-Control-Expose-Headers: Content-Type, X-Auth-Token, X-Request-ID, Location, Etag, ETag
< Access-Control-Allow-Headers: Content-Type, Depth, User-Agent, X-File-Size, X-Requested-With, If-Modified-Since, X-File-Name, Cache-Control, X-Auth-Token, If-Match
< Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS, HEAD
< Access-Control-Allow-Origin: *
< X-Request-ID: 5ad53536debfff23f55641caecb3849d
< Content-Length: 0
< Date: Fri, 01 Jun 2012 22:19:11 GMT
< Server: Cowboy
< Connection: keep-alive
<
* Connection #0 to host api.2600hz.com left intact
* Closing connection #0
* SSLv3, TLS alert, Client hello (1):
```

# Reverse Proxies

Apache, nginx, HAProxy, and others can be used for SSL/TLS termination instead of Crossbar itself.

Ideally on a separate server (or two), these can be setup to load balance across multiple instances of Crossbar in your cluster.

## Apache

In `httpd.conf` add `Listen 8443` (or whatever port you want clients to connect on with TLS).

Add this virtual host:

```
<VirtualHost *:8443>
    ServerName api.your.domain.com:8443
    ProxyPreserveHost On

    SSLEngine on
    SSLCertificateKeyFile "/etc/path/to/privkey.pem"
    SSLCertificateFile "/etc/path/to/cert.pem"

    # Servers to proxy the connection, or;
    # List of application servers:
    # Usage:
    # ProxyPass / http://[IP Addr.]:[port]/
    # ProxyPassReverse / http://[IP Addr.]:[port]/
    # Example:
    ProxyPass / http://crossbar.server:8000/
    ProxyPassReverse / http://crossbar.server:8000/
</VirtualHost>
```

Save the virtual host and restart httpd/apache2.

!!! note
    This is just a basic example. Other configurations are likely more efficient and better suited to a production environment.
