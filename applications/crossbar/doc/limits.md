
### Limits

#### Get limits for a given account

##### Request

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/limits`
- Payload: None

##### Response

```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 0,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "status": "success"
}
```

#### Update limits for a given account

##### Request VERSION 1

- Verb: `POST`
- Url: `v1/accounts/{{ACCOUNT_ID}}/limits`
- Payload:

```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "allow_prepay": true,
        "outbound_trunks": 5
    }
}
```

##### Response VERSION 1


```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "ui_metadata": {
            "version": "v3.19_s3",
            "ui": "monster-ui"
        },
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "status": "success",
}
```

##### Request VERSION 2

The version 2 will respond with a "402 - accept charges" first. You will then need to re-do the same request and add `"accept_charges": true` to your payload (see below).

- Verb: `POST`
- Url: `v2/accounts/{{ACCOUNT_ID}}/limits`
- Payload:

```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "allow_prepay": true,
        "outbound_trunks": 5
    }
}
```

###### Accepting charges

```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "accept_charges": true
}
```

##### Response VERSION 2


```
{
    "data": {
        "limits": {
            "inbound_trunks": {
                "category": "limits",
                "item": "inbound_trunks",
                "quantity": 11,
                "rate": 6.9900000000000002132,
                "single_discount": true,
                "single_discount_rate": 0.0,
                "cumulative_discount": 0,
                "cumulative_discount_rate": 0.0
            }
        }
    },
    "error": "402",
    "message": "accept charges",
    "status": "error",
}
```

###### Once charges are accepted

```
{
    "data": {
        "twoway_trunks": 0,
        "inbound_trunks": 11,
        "id": "limits",
        "ui_metadata": {
            "version": "v3.19_s3",
            "ui": "monster-ui"
        },
        "allow_prepay": true,
        "outbound_trunks": 5
    },
    "status": "success",
}
```
