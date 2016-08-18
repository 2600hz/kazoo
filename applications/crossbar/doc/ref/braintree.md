### Braintree

#### About Braintree

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/cards

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/customer

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/customer

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions/{ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{ID}
```

