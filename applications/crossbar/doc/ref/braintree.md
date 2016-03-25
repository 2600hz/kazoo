### Braintree

#### About Braintree

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/credits

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/credits

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/cards

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/customer

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/customer

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions/{ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

