# Braintree

## About Braintree

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/client_token

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/client_token
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/cards

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/customer

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/customer

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions/{TRANSACTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions/{TRANSACTION_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

