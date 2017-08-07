### Authentication

#### About Authentication

#### Schema



#### Patch

> PATCH /v2/auth/identity_secret

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/identity_secret
```

#### Fetch

> GET /v2/auth/apps

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps
```

#### Fetch

> GET /v2/auth/providers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers
```

#### Fetch

> GET /v2/auth/links

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

#### Create

> PUT /v2/auth/callback

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/callback
```

#### Create

> PUT /v2/auth/authorize

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/authorize
```

#### Fetch

> GET /v2/auth/tokeninfo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo
```

#### Change

> POST /v2/auth/tokeninfo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo
```

#### Fetch

> GET /v2/auth/keys/public

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/public
```

#### Patch

> PATCH /v2/auth/keys/private

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/private
```

#### Fetch

> GET /v2/auth/apps/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Change

> POST /v2/auth/apps/{APP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Remove

> DELETE /v2/auth/apps/{APP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Fetch

> GET /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Change

> POST /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Remove

> DELETE /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Fetch

> GET /v2/auth/links/{LINK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

#### Create

> PUT /v2/auth/links/{LINK_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

#### Remove

> DELETE /v2/auth/links/{LINK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

