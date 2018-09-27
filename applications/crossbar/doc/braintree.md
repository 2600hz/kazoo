### Braintree

#### About Braintree

DISCLAIMER: Please read the [docs available at braintree](https://developers.braintreepayments.com/) to know how braintree works and how its APIs are intended to work.

Braintree provides good docs to make you knowledgeable about what are important fields and what fields can be left from the API requests.

This doc just provides examples as what is possible with the API but you should consult braintree and their API documentation before using kazoo's braintree modules.

Test out braintree using sandbox account before deploying it in production as the module is considered *NOT PRODUCTION READY*.

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/braintree/client_token

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/client_token
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "client_token": "{BRAINTREE_CLIENT_TOKEN}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Show this account's credits

> GET /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "amount": 500.0,
        "billing_account_id": "31a05ddba6a9df166c7d50fc4b683606"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Add credit to this account

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/credits

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"amount":200.00}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/credits
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "bookkeeper_info": {
            "add_ons": [],
            "amount": "20.00",
            "avs_postal_response": "M",
            "avs_street_response": "I",
            "card": {
                "bin": "411111",
                "card_type": "Visa",
                "customer_location": "US",
                "default": false,
                "expiration_month": "11",
                "expiration_year": "2020",
                "expired": false,
                "id": "7gz3qw",
                "last_four": "1111"
            },
            "ccv_response_code": "I",
            "created_at": "2016-09-29T14:22:54Z",
            "currency_code": "USD",
            "customer": {
                "company": "ACME Corp",
                "first_name": "John",
                "id": "31a05ddba6a9df166c7d50fc4b657854",
                "last_name": "Doe",
                "phone": "9122475533"
            },
            "discounts": [],
            "id": "0gmy6jrw",
            "is_api": true,
            "is_automatic": false,
            "is_recurring": false,
            "merchant_account_id": "romanat",
            "order_id": "a6268d1a31a76d53857fae0d",
            "processor_authorization_code": "XJ3YL9",
            "processor_response_code": "1000",
            "processor_response_text": "Approved",
            "purchase_order": "3001",
            "status": "submitted_for_settlement",
            "tax_exempt": false,
            "type": "sale",
            "update_at": "2016-09-29T14:22:54Z"
        },
        "description": "credit addition from credit card",
        "id": "80591de5690374ebba7adc4ffaaf51c1",
        "order_id": "a6268d1a31a76d53857fae0d310552ab",
        "sub_account_id": "22f1b082e505064cc930e944bf9a0728",
        "sub_account_name": "romana"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### List this account's transactions

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [{
        "id": "{TRANSACTION_ID}",
        "status": "{STATUS_OF_TRANSACTION}",
        "type": "sale",
        "currency_code": "{CURRENCY_CODE}",
        "amount": "20.00",
        "merchant_account_id": "{BRAINTREE_MERCHANT_ID}",
        "order_id": "{ORDER_ID}",
        "purchase_order": "3001",
        "created_at": "2016-09-29T14:22:54Z",
        "update_at": "2016-09-29T14:22:54Z",
        "avs_postal_response": "M",
        "avs_street_response": "I",
        "ccv_response_code": "I",
        "processor_authorization_code": "XJ3LR9",
        "processor_response_code": "1000",
        "processor_response_text": "Approved",
        "tax_exempt": false,
        "billing_address": {
            {BRAINTREE_CUSTOMER_ADDRESS}
        },
        "shipping_address": {
            {BRAINTREE_CUSTOMER_SHIPPING_ADDRESS}
        },
        "customer": {
            {BRAINTREE_CUSTOMER_INFO}
        },
        "card": {
            {BRAINTREE_CREDIT_CARD_DETAILS}
        },
        "add_ons": [
            {ADDONS_IN_ORDER}
        ],
        "discounts": [
            {DISCOUNTS_APPLIED_TO_ORDER}
        ],
        "is_api": true,
        "is_automatic": {AUTOMATIC_BILLING},
        "is_recurring": {RECURRING_SUBSCRIPTION}
    }],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### List addresses

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "company": "{CUSTOMER_COMPANY}",
            "country_code": "{BRAINTREE_COUNTRY_CODE}",
            "country_code_three": "{BRAINTREE_COUNTRY_CODE_THREE}",
            "country_code_two": "{BRAINTREE_COUNTRY_CODE_TWO}",
            "country_name": "{BRAINTREE_COUNTRY_NAME}",
            "created_at": "2016-09-23T00:20:51Z",
            "customer_id": "{ACCOUNT_ID}",
            "extended_address": "{EXTENDED_CUSTOMER_ADDRESS}",
            "first_name": "{CUSTOMER_FIRST_NAME}",
            "id": "7x",
            "last_name": "{CUSTOMER_LAST_NAME}",
            "locality": "{CUSTOMER_LOCALITY}",
            "postal_code": "{CUSTOMER_POSTAL_CODE}",
            "region": "{BRAINTREE_REGION}",
            "street_address": "{CUSTOMER_ADDRESS}",
            "updated_at": "2016-09-23T00:20:51Z"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Add an address

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/addresses

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"{ADDRESS_INFORMATION}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "company": "{CUSTOMER_COMPANY}",
        "country_code": "{BRAINTREE_COUNTRY_CODE}",
        "country_code_three": "{BRAINTREE_COUNTRY_CODE_THREE}",
        "country_code_two": "{BRAINTREE_COUNTRY_CODE_TWO}",
        "country_name": "{BRAINTREE_COUNTRY_NAME}",
        "created_at": "2016-09-23T00:20:51Z",
        "customer_id": "{ACCOUNT_ID}",
        "extended_address": "{EXTENDED_CUSTOMER_ADDRESS}",
        "first_name": "{CUSTOMER_FIRST_NAME}",
        "id": "7x",
        "last_name": "{CUSTOMER_LAST_NAME}",
        "locality": "{CUSTOMER_LOCALITY}",
        "postal_code": "{CUSTOMER_POSTAL_CODE}",
        "region": "{BRAINTREE_REGION}",
        "street_address": "{CUSTOMER_ADDRESS}",
        "updated_at": "2016-09-23T00:20:51Z"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### List credit cards

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [{
        "id": "{CARD_ID}",
        "bin": "{CARD_FIRST_SIX_DIGITS}",
        "card_type": "Visa",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "default": true,
        "expiration_month": "11",
        "expiration_year": "2020",
        "expired": false,
        "customer_location": "US",
        "last_four": "1111",
        "customer_id": "{ACCOUNT_ID}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "billing_address": {
            {BRAINTREE_ADDRESS}
        },
        "billing_address_id": "{BRAINTREE_ADDRESS_ID}"
    }],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Add a credit card

> PUT /v2/accounts/{ACCOUNT_ID}/braintree/cards

To add a credit card, the information about a credit card can be sent in the request or `payment_token_nonce`.

##### with payment method nonce

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"payment_method_nonce":"valid-nonce"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{CARD_ID}",
        "bin": "{CARD_FIRST_SIX_DIGITS}",
        "card_type": "Visa",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "default": true,
        "expiration_month": "11",
        "expiration_year": "2020",
        "expired": false,
        "customer_location": "US",
        "last_four": "1111",
        "customer_id": "{ACCOUNT_ID}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "billing_address": {
            {BRAINTREE_ADDRESS}
        },
        "billing_address_id": "{BRAINTREE_ADDRESS_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### with credit card info

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"{CREDIT_CARD_INFO}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{CARD_ID}",
        "bin": "{CARD_FIRST_SIX_DIGITS}",
        "card_type": "Visa",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "default": true,
        "expiration_month": "11",
        "expiration_year": "2020",
        "expired": false,
        "customer_location": "US",
        "last_four": "1111",
        "customer_id": "{ACCOUNT_ID}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "billing_address": {
            {BRAINTREE_ADDRESS}
        },
        "billing_address_id": "{BRAINTREE_ADDRESS_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Get this account's details

> GET /v2/accounts/{ACCOUNT_ID}/braintree/customer

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{ACCOUNT_ID}",
        "first_name": "John",
        "last_name": "Doe",
        "company": "Presentation",
        "phone": "9122475533",
        "created_at": "2016-09-17T21:08:01Z",
        "updated_at": "2016-09-23T00:20:53Z",
        "credit_cards": [{
            {BRAINTREE_CREDIT_CARD}
        }],
        "addresses": [{
            {BRAINTREE_CUSTOMER_ADDRESS}
        }]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Add a customer

> POST /v2/accounts/{ACCOUNT_ID}/braintree/customer

To add a customer we can send the customer's info as with just customer's name, company and phone or can add a `payment_method_nonce` with it,
or add a credit card with the customer info with card's info or with `payment_method_nonce` token.

##### the user can be added without any credit card

```json
{
    "data": {
        "first_name": "John",
        "last_name": "Doe",
        "company": "ACME CORP",
        "phone": "6000000000"
    }
}
```

##### without any credit card and contains payment method nonce in their json request

```json
{
    "data": {
        "first_name": "John",
        "last_name": "Doe",
        "company": "ACME CORP",
        "phone": "6000000000",
        "payment_method_nonce": "valid-nonce"
    }
}
```

##### payment method nonce is added to the credit card section

```json
{
    "data": {
        "first_name": "John",
        "last_name": "Doe",
        "company": "ACME CORP",
        "phone": "6000000000",
        "credit_card":{
            "payment_method_nonce": "valid-nonce"
        }
    }
}
```

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"{CUSTOMER_INFO}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/customer
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{ACCOUNT_ID}",
        "first_name": "John",
        "last_name": "Doe",
        "company": "Presentation",
        "phone": "9122475533",
        "created_at": "2016-09-17T21:08:01Z",
        "updated_at": "2016-09-23T00:20:53Z",
        "credit_cards": [{
            {BRAINTREE_CREDIT_CARD}
        }],
        "addresses": [{
            {BRAINTREE_CUSTOMER_ADDRESS}
        }]
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch details on a transaction

> GET /v2/accounts/{ACCOUNT_ID}/braintree/transactions/{TRANSACTION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/transactions/{TRANSACTION_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{TRANSACTION_ID}",
        "status": "{STATUS_OF_TRANSACTION}",
        "type": "sale",
        "currency_code": "{CURRENCY_CODE}",
        "amount": "20.00",
        "merchant_account_id": "{BRAINTREE_MERCHANT_ID}",
        "order_id": "{ORDER_ID}",
        "purchase_order": "3001",
        "created_at": "2016-09-29T14:22:54Z",
        "update_at": "2016-09-29T14:22:54Z",
        "avs_postal_response": "M",
        "avs_street_response": "I",
        "ccv_response_code": "I",
        "processor_authorization_code": "XJ3LR9",
        "processor_response_code": "1000",
        "processor_response_text": "Approved",
        "tax_exempt": false,
        "billing_address": {
            {BRAINTREE_CUSTOMER_ADDRESS}
        },
        "shipping_address": {
            {BRAINTREE_CUSTOMER_SHIPPING_ADDRESS}
        },
        "customer": {
            {BRAINTREE_CUSTOMER_INFO}
        },
        "card": {
            {BRAINTREE_CREDIT_CARD_DETAILS}
        },
        "add_ons": [
            {ADDONS_IN_ORDER}
        ],
        "discounts": [
            {DISCOUNTS_APPLIED_TO_ORDER}
        ],
        "is_api": true,
        "is_automatic": {AUTOMATIC_BILLING},
        "is_recurring": {RECURRING_SUBSCRIPTION}
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove an address

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch an address' details

> GET /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "7x",
        "customer_id": "{ACCOUNT_ID}",
        "first_name": "{CUSTOMER_FIRST_NAME}",
        "last_name": "{CUSTOMER_LAST_NAME}",
        "company": "{CUSTOMER_COMPANY}",
        "street_address": "{CUSTOMER_ADDRESS}",
        "extended_address": "{EXTENDED_CUSTOMER_ADDRESS}",
        "locality": "{CUSTOMER_LOCALITY}",
        "region": "{BRAINTREE_REGION}",
        "postal_code": "{CUSTOMER_POSTAL_CODE}",
        "country_code": "{BRAINTREE_COUNTRY_CODE}",
        "country_code_two": "{BRAINTREE_COUNTRY_CODE_TWO}",
        "country_code_three": "{BRAINTREE_COUNTRY_CODE_THREE}",
        "country_name": "{BRAINTREE_COUNTRY_NAME}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-23T00:20:51Z"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Update an address

> POST /v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"{ADDRESS_INFORMATION}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/addresses/{ADDRESS_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "7x",
        "customer_id": "{ACCOUNT_ID}",
        "first_name": "{CUSTOMER_FIRST_NAME}",
        "last_name": "{CUSTOMER_LAST_NAME}",
        "company": "{CUSTOMER_COMPANY}",
        "street_address": "{CUSTOMER_ADDRESS}",
        "extended_address": "{EXTENDED_CUSTOMER_ADDRESS}",
        "locality": "{CUSTOMER_LOCALITY}",
        "region": "{BRAINTREE_REGION}",
        "postal_code": "{CUSTOMER_POSTAL_CODE}",
        "country_code": "{BRAINTREE_COUNTRY_CODE}",
        "country_code_two": "{BRAINTREE_COUNTRY_CODE_TWO}",
        "country_code_three": "{BRAINTREE_COUNTRY_CODE_THREE}",
        "country_name": "{BRAINTREE_COUNTRY_NAME}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-23T00:20:51Z"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Remove a credit card

> DELETE /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": { },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Fetch a credit card's details

> GET /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{CARD_ID}",
        "bin": "{CARD_FIRST_SIX_DIGITS}",
        "card_type": "Visa",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "default": true,
        "expiration_month": "11",
        "expiration_year": "2020",
        "expired": false,
        "customer_location": "US",
        "last_four": "1111",
        "customer_id": "{ACCOUNT_ID}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "billing_address": {
            {BRAINTREE_ADDRESS}
        },
        "billing_address_id": "{BRAINTREE_ADDRESS_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Update a credit card

> POST /v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"{CREDIT_CARD_INFO_OR_PAYMENT_NONCE}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/braintree/cards/{CARD_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{CARD_ID}",
        "bin": "{CARD_FIRST_SIX_DIGITS}",
        "card_type": "Visa",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "default": true,
        "expiration_month": "11",
        "expiration_year": "2020",
        "expired": false,
        "customer_location": "US",
        "last_four": "1111",
        "customer_id": "{ACCOUNT_ID}",
        "created_at": "2016-09-23T00:20:51Z",
        "updated_at": "2016-09-29T14:22:54Z",
        "billing_address": {
            {BRAINTREE_ADDRESS}
        },
        "billing_address_id": "{BRAINTREE_ADDRESS_ID}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
