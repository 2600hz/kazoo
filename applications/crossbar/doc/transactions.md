

### Transactions

#### Get transactions

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/transactions`
- Payload: None
- options:
    - `created_from` {{TIMESTAMP}}
    - `created_to` {{TIMESTAMP}}
    - `reason` `only_calls`

##### Response

    {"data": [
        {"description": "monthly rollup",
         "id": "09dd02e20e07dbb65401802ba20cfb32",
         "amount": 10.179999999999999716,
         "reason": "database_rollup",
         "type": "credit",
         "created": 63598331974,
         "version": 2,
         "code": 9999
        }
        ,{"metadata": {
            "auth_account_id": "{{AUTH_ACCOUNT_ID}}"
        },
        "id": "7dd1c20894587e9cbacb2d7fa2de80ab",
        "amount": 1.0,
        "reason": "admin_discretion",
        "type": "debit",
        "created": 63598591394,
        "version": 2,
        "code": 3005
     }],
     "status": "success"
    }


#### Get current balance

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/transactions/current_balance`
- Payload: None

##### Response

    {"data": {
        "balance": 9.18
     },
     "status": "success"
    }

#### Debit an account

Only for super duper admins and resellers.

##### Request

- Verb: `DELETE`
- Url: `/accounts/{{ACCOUNT_ID}}/transactions/debit`
- Payload: `{"data": {"amount": 1} }`

##### Response

    {"data": {
        "metadata": {
            "auth_account_id": "{{ACCOUNT_ID}}"
        },
        "id": "d478a0f74865c8512b71daf82b602b7a",
        "amount": 1.0,
        "reason": "admin_discretion",
        "type": "debit",
        "created": 63598603319,
        "version": 2,
        "code": 3005
     },
     "status": "success"
    }
