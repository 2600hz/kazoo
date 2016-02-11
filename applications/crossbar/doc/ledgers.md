/*
Section: Crossbar
Title: Ledgers
Language: en-US
Version: 3.22
*/

# Ledgers

## List current Ledgers

List current ledgers and value for an account.

### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers`


### Response

```json
{
    "data": {
        "test": 10,
        "support": 17
    },
    "status": "success"
}
```

## Get Ledger value

List current ledgers and value for an account.

### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers/{{LEDGER}}`


### Response

```json
{
    "data": {
        "{{LEDGER}}": 17
    },
    "status": "success"
}
```

## Credit / Debit

Credit or Debit a specific ledger (**must be admin to use**).

### Request

- Verb: `PUT`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers/credit` or `/accounts/{{ACCOUNT_ID}}/ledgers/debit`
- Payload:
    - `amount` *integer*: is mandatory and must be > 0
    - `description` *string*: not mandatory

```json
{
    "amount": 100,
    "description": "blablabla",
    "source": {
        "service": "tower/support/...",
        "id": "mac/mdn/..."
    },
    "usage": {
        "type": "data",
        "quantity": 5,
        "unit": "MB"
    },
    "period": {
        "start": 10938710938,
        "end": 214109238023899
    },
    "account": {
        "id": "390820938109diadiuw",
        "name": "Account Name"
    }
}
```

### Response

```json
{
    "data": {
        "amount": 100,
        "description": "blablabla",
        "source": {
            "service": "tower/support/...",
            "id": "mac/mdn/..."
        },
        "usage": {
            "type": "data",
            "quantity": 5,
            "unit": "MB"
        },
        "period": {
            "start": 10938710938,
            "end": 214109238023899
        },
        "account": {
            "id": "390820938109diadiuw",
            "name": "Account Name"
        }
    },
    "status": "success"
}
```
