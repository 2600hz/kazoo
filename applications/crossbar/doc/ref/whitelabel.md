# Whitelabel

## About Whitelabel

#### Schema

Whitelabel settings



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`company_name` | The company name to display to users | `string()` |   | `false` | `supported`
`domain` | This is the whitelabeled domain that users will be entering to reach the UI | `string()` |   | `false` | `supported`
`fake_api_url` | This is a whitelabeled API URL, primarily used by the developer application | `string()` |   | `false` | `beta`
`hide_credits` | When checked this hides the credits | `boolean()` | `false` | `false` | `beta`
`hide_powered` | When checked this hides the powered by 2600Hz on the bottom right | `boolean()` | `false` | `false` | `supported`
`hide_registration` | When checked this hides the ability to register for a new account | `boolean()` | `false` | `false` | `beta`
`inbound_trunks_price` | The price to show for inbound trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`
`nav.help` | The URL to use when the help link is clicked | `string()` |   | `false` | `supported`
`nav.learn_more` | The URL to use when the 'Learn More!' link is clicked | `string()` |   | `false` | `supported`
`nav` | Properties related to navigation in the UI | `object()` |   | `false` |  
`outbound_trunks_price` | The price to show for outbound trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`
`port.authority` | The account ID(s) to be used for administrating port requests | `string() | array(string())` |   | `false` | `supported`
`port.features` | The URL to use when the features link is clicked | `string()` |   | `false` | `supported`
`port.loa` | The URL to use when the LOA link is clicked | `string()` |   | `false` | `supported`
`port.resporg` | The URL to use when the resporg link is clicked | `string()` |   | `false` | `supported`
`port.support_email` | The support email address to display to the user | `string()` |   | `false` | `supported`
`port.terms` | The URL to use when the terms and conditions link is clicked | `string()` |   | `false` | `supported`
`port` | Parameters related to white-labeling port requests | `object()` |   | `false` |  
`twoway_trunks_price` | The price to show for twoway trunks, this is currently only for display purposes | `string()` |   | `false` | `beta`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/whitelabel

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/whitelabel

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/domains

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel/domains

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/domains
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel/welcome

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/welcome
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/icon

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/icon
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel/icon

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/icon
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/logo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/logo
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/whitelabel/logo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/logo
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/welcome

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/welcome
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/icon

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/icon
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/logo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/whitelabel/{WHITELABEL_DOMAIN}/logo
```

