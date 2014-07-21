SSO guide

(external)

### Overview

**SSO (Single Sign-On)** is a part of *ubic* infrastructure responsible for authentication of users across multiple applications. This guide describes the integration of external applications with the SSO Rest API.

The full specification for the APIs is available [here][sso_api_spec].

The latest version of the API is available at the following address:
https://sso.ubnt.com/api/sso/v1/about

Please note that to enable cross-origin API calls ([CORS][cors]), your application URI needs to be added to the SSO configuration. Currently supported domains:

 * https://store.ubnt.com
 * https://account.ubnt.com

### API version
``` bash
$ curl -i  https://sso.ubnt.com/api/sso/v1/about
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Wed, 25 Sep 2013 15:22:46 GMT
server: Cowboy
Content-Length: 26
Connection: keep-alive

{"version":"1.0.0-beta17"}%
```

### Legal information
```bash
$ curl -i  https://sso.ubnt.com/api/sso/v1/legal
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Wed, 25 Sep 2013 15:24:57 GMT
server: Cowboy
Content-Length: 59
Connection: keep-alive

{"rev_terms":"REV2013-01-18","rev_privacy":"REV2013-01-18"}%
```

### User registration process

1. Register the user by posting user data to the /user resource:
```bash
$ curl -i -X POST -H "Content-Type: application/json" -d '{"email": "marcin.gornik@ubnt.com", "username": "marcin_gornik", "password": "marcin123", "first_name": "Marcin", "last_name": "Gornik", "curr_terms_rev": "REV2013-01-18", "curr_privacy_rev": "REV2013-01-18"}' https://sso.ubnt.com/api/sso/v1/user
```
```http
HTTP/1.1 201 Created
date: Thu, 26 Sep 2013 12:18:41 GMT
location: https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 12:18:41 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 0
Connection: keep-alive
```
This sends the verification email to the specified email account. The API call returns a location of the user data, containing the UUID that was assigned to the created user:
```
https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
```
1. Getting user data (unauthorized call):
```bash
$ curl -i https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
```
```http
HTTP/1.1 401 Unauthorized
date: Thu, 26 Sep 2013 12:22:46 GMT
server: Cowboy
www-authenticate: ubnt_auth
Content-Length: 0
Connection: keep-alive
```
1. Getting user data (authorization using the provided cookie):
```bash
$ curl -i --cookie "UBIC_AUTH=7e317..." https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Thu, 26 Sep 2013 12:39:07 GMT
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 12:39:07 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 12:39:07 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 444
Connection: keep-alive

{"fields_missing":["security_question","security_answer"],"accounts":{"lithium":"8cc50bdf-d46d-442e-b8c8-413a2b058891","magento":"d3d3161e-afb2-419b-8734-b3d3ee3f75ae"},"curr_privacy_rev":"REV2013-01-18","curr_terms_rev":"REV2013-01-18","is_verified":false,"time_created":"2013-09-26T12:18:41Z","last_name":"Gornik","first_name":"Marcin","email":"marcin.gornik@ubnt.com","username":"marcin_gornik","uuid":"93c5adfd-8c4b-456d-ac55-dc28b8b26d05"}%
```
1. To finish the registration process, user needs to click the link provided in the email sent to the specified email account:
```
https://account.ubnt.com/#verify/verification-code/826fc9dd-11a6-47f3-99a0-ead777ec6d4c
```
1. After clicking the link in the email, the `is_verified` field in user data is set to `true`:
```bash
$ curl -i --cookie "UBIC_AUTH=7e317..." https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Thu, 26 Sep 2013 12:39:07 GMT
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 12:39:07 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 12:39:07 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 444
Connection: keep-alive

{"fields_missing":["security_question","security_answer"],"accounts":{"lithium":"8cc50bdf-d46d-442e-b8c8-413a2b058891","magento":"d3d3161e-afb2-419b-8734-b3d3ee3f75ae"},"curr_privacy_rev":"REV2013-01-18","curr_terms_rev":"REV2013-01-18","is_verified":true,"time_created":"2013-09-26T12:18:41Z","last_name":"Gornik","first_name":"Marcin","email":"marcin.gornik@ubnt.com","username":"marcin_gornik","uuid":"93c5adfd-8c4b-456d-ac55-dc28b8b26d05"}%
```

### Authentication

#### Logging in
The login resource can be used to obtain a valid cookie for the user, provided that proper credentials are sent:

```bash
$ curl -i -X POST -H "Content-Type: application/json"  -d '{"user": "marcin_gornik", "password": "marcin123"}' 
https://sso.ubnt.com/api/sso/v1/login
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Thu, 26 Sep 2013 13:40:58 GMT
location: https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 13:40:58 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 443
Connection: keep-alive

{"fields_missing":["security_question","security_answer"],"accounts":{"lithium":"8cc50bdf-d46d-442e-b8c8-413a2b058891","magento":"d3d3161e-afb2-419b-8734-b3d3ee3f75ae"},"curr_privacy_rev":"REV2013-01-18","curr_terms_rev":"REV2013-01-18","is_verified":true,"time_created":"2013-09-26T12:18:41Z","last_name":"Gornik","first_name":"Marcin","email":"marcin.gornik@ubnt.com","username":"marcin_gornik","uuid":"93c5adfd-8c4b-456d-ac55-dc28b8b26d05"}%
```

#### Logout
There is no server-side session for a logged in user, so the purpose of the logout is just to destroy a cookie that was provided during the login.

```bash
$ curl -i -X POST -H "Content-Type: application/json" --cookie "UBIC_AUTH=7e317..." -d '{"user": "marcin_gornik"}' https://sso.ubnt.com/api/sso/v1/logout
```
```http
HTTP/1.1 204 No Content
Content-length: 0
date: Thu, 26 Sep 2013 13:43:47 GMT
server: Cowboy
set-cookie: UBIC_AUTH=deleted; Version=1; Expires=Thu, 01-Jan-1970 00:00:01 GMT; Max-Age=0; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: lithiumRest:ubnt=~2mk73vlh4UlSCbz5I~lGLL7XGlr0ebwdgtZ0aueTEA6PrYy_rlVQEJSy12n9Wl2admLGwvOVDWM20AuVwb7ek7QOrNR9aMNp29VD4Krznh9L9slzgk27uG18wZVE7pfoCeWw-gFqNeo8MRNZeTky-vm-b_Mim9oFov_RY8Vg8QmXStgd5q_4fkvTgsbwNqH2xs-jfzP4cHWX6gl7pRW9WsAEXSJmfp4dzXBCOosgyHg3VIsR4LGzW056Gjgdrn20bhn6zkyR1mD8tXxqVH32FFxclSuf1uSG5fyuhqLJqZqBos4ey5t4Z0cXttkpKmtbkrDi9etnzzdtdoqmyVq30z0CON-qdMc6j7bKZCHQ..; Version=1; Domain=.ubnt.com; Path=/
set-cookie: lithiumSSO:ubnt=~2Qf9nDeovII6uozcJ~RdkiSOO6sN9Em2xar_Vz4p4Ngyc3fuK3T0agx-UD7cMvx84LpiIIue9JGcK8_EE853OMazcGulSbFJH9-02uOXtF29CHCPXnTarHyGEJBUblvZZPBoM2imiBr4RMxhc_cON1t6Ym961dT7e38y9IM_407aqeHnlpiHgtrWkyzitad_37LYIcc5rebgbLjj7P_YQfLqZ0L0isCLpU4Mg_tzqhZBMI2HCqyNnFxLt57ZemNG9lg_iELDRLAKWjlJBx7nFS-efXl1Kz0BQ9fxpt2I09CI4Z5G9mE8ywtq8laYEBaYEVgQ6_cLniTpdgFfDnHoKxVlauE5fC6iZ5vtBfCA..; Version=1; Domain=.ubnt.com; Path=/
Connection: keep-alive
```

#### Verifying cookie
To verify if the provided cookie is valid, try using it to get user data:

```bash
$ curl -i --cookie "UBIC_AUTH=7e317..." https://sso.ubnt.com/api/sso/v1/user/93c5adfd-8c4b-456d-ac55-dc28b8b26d05
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Thu, 26 Sep 2013 14:02:29 GMT
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 14:02:29 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 14:02:29 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 443
Connection: keep-alive

{"fields_missing":["security_question","security_answer"],"accounts":{"lithium":"8cc50bdf-d46d-442e-b8c8-413a2b058891","magento":"d3d3161e-afb2-419b-8734-b3d3ee3f75ae"},"curr_privacy_rev":"REV2013-01-18","curr_terms_rev":"REV2013-01-18","is_verified":true,"time_created":"2013-09-26T12:18:41Z","last_name":"Gornik","first_name":"Marcin","email":"marcin.gornik@ubnt.com","username":"marcin_gornik","uuid":"93c5adfd-8c4b-456d-ac55-dc28b8b26d05"}%
```

To avoid storing the UUID of the user (which is required to get the location of user data), you can use a /user/self alias, the SSO will determine the user based on the cookie content:

```bash
$ curl -i --cookie "UBIC_AUTH=7e317..." https://sso.ubnt.com/api/sso/v1/user/self
```
```http
HTTP/1.1 200 OK
content-type: application/json
date: Thu, 26 Sep 2013 14:02:29 GMT
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 14:02:29 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 14:02:29 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Content-Length: 443
Connection: keep-alive

{"fields_missing":["security_question","security_answer"],"accounts":{"lithium":"8cc50bdf-d46d-442e-b8c8-413a2b058891","magento":"d3d3161e-afb2-419b-8734-b3d3ee3f75ae"},"curr_privacy_rev":"REV2013-01-18","curr_terms_rev":"REV2013-01-18","is_verified":true,"time_created":"2013-09-26T12:18:41Z","last_name":"Gornik","first_name":"Marcin","email":"marcin.gornik@ubnt.com","username":"marcin_gornik","uuid":"93c5adfd-8c4b-456d-ac55-dc28b8b26d05"}%
```

### Searching
The API allows checking if users exists using username or email address:

#### By username
```bash
$ curl -i -X HEAD https://sso.ubnt.com/api/sso/v1/search/username/marcin_gornik
```
```http
HTTP/1.1 204 No Content
Content-length: 0
date: Thu, 26 Sep 2013 14:20:17 GMT
server: Cowboy
Connection: keep-alive
```

```bash
$ curl -i -X HEAD https://sso.ubnt.com/api/sso/v1/search/username/does_not_exist
```
```http
HTTP/1.1 404 Not Found
Content-length: 0
date: Thu, 26 Sep 2013 14:22:55 GMT
server: Cowboy
Connection: keep-alive
```

#### By email

```bash
$ curl -i -X HEAD https://sso.ubnt.com/api/sso/v1/search/email/marcin.gornik@ubnt.com
```
```http
HTTP/1.1 204 No Content
Content-length: 0
date: Thu, 26 Sep 2013 14:24:20 GMT
server: Cowboy
Connection: keep-alive
```

```bash
$ curl -i -X HEAD https://sso.ubnt.com/api/sso/v1/search/email/some_email@example.com
```
```http
HTTP/1.1 404 Not Found
Content-length: 0
date: Thu, 26 Sep 2013 14:24:46 GMT
server: Cowboy
Connection: keep-alive
```

### Password change

Password change is handled just like a regular user data update, i.e/ by using PUT method to store new user data:

```bash
$ curl -i -X PUT -H "Content-Type: application/json" -d '{"username": "marcin_gornik", "password": "new_password"}'  --cookie "UBIC_AUTH=7e317..." https://sso.ubnt.com/api/sso/v1/user/self
```
```http
HTTP/1.1 204 No Content
Content-length: 0
date: Thu, 26 Sep 2013 16:09:27 GMT
server: Cowboy
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 16:09:26 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
set-cookie: UBIC_AUTH=7e317...; Version=1; Expires=Thu, 03-Oct-2013 16:09:27 GMT; Max-Age=604800; Domain=.ubnt.com; Path=/; Secure; HttpOnly
Connection: keep-alive
```

[sso_api_spec]: https://github.com/Ubiquiti-Cloud/ubic/blob/develop/doc/sso_rest_api/SSO_REST_API_SPEC_v1.0.0.md
[cors]: http://en.wikipedia.org/wiki/Cross-origin_resource_sharing
