/*
Section: Crossbar
Title: Ledgers
Language: en-US
Version: 3.22
*/

### Ledgers

#### List current Ledgers

List current ledgers and value for an account.

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers`


##### Response

```json
{
    "data": {
        "test": 10,
        "support": 17
    },
    "status": "success"
}
```

#### Get Ledger value

List current ledgers and value for an account.

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers/{{LEDGER}}`


##### Response

```json
{
    "data": {
        "{{LEDGER}}": 17
    },
    "status": "success"
}
```

#### Credit / Debit

Credit or Debit a specific ledger (**must be admin to use**).

##### Request

- Verb: `POST`
- Url: `/accounts/{{ACCOUNT_ID}}/ledgers/{{LEDGER}}/credit` or `/accounts/{{ACCOUNT_ID}}/ledgers/{{LEDGER}}/debit`
- Payload:
    - `amount` *integer*: is mandatory and must be > 0
    - `description` *string*: not mandatory

```json
{
    "data": {
        "amount": {{AMOUNT}},
        "description": "{{DESC}}"
    }
}
```

##### Response

```json
{
    "data": {
        "name": "{{LEDGER}}",
        "amount": "{{AMOUNT}}",
        "description": "{{DESC}}",
        "account_id": "{{ACCOUNT_ID}}",
        "type": "credit"
    },
    "status": "success"
}
```
