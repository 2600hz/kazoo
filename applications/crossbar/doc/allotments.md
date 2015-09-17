/*
Section: Crossbar
Title: Allotments
Language: en-US
Version: 3.22
*/

# Allotments

Module `cb_allotments`.

## Get allotments configuration for a given account

### Request

- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/allotments`
- Payload: None

### Response

```JSON
{
    "data": {
        "outbound_national": {
            "amount": 600,
            "cycle": "hourly",
            "increment": 60,
            "minimum": 60,
            "no_consume_time": 2,
            "group_consume": [
                "outbound_local"
            ]
        },
        "outbound_local": {
            "amount": 600,
            "cycle": "hourly",
            "increment": 60,
            "minimum": 60,
            "no_consume_time": 2,
            "group_consume": [
                "outbound_national"
            ]
        }
    },
    "status": "success"
}
```

#### Explanаtion

Each object have name (`outbound_national`) which build from direction (inbound or outbound) and classificator from number_manager configuration.
Properties:
- `amount`: time in seconds. which can be consumed
- `cycle`: when we reset consumed allotments counter, must be one of `minutely`, `hourly`, `daily`, `weekly`, `monthly`.
- `increment`: step (in seconds) of incrementing counter, minimum 1 second
- `minimum`: minimum ammount added to counter
- `no_consume_time`: if call less or equal of this time (in seconds), then no allotment consumed.
- `group_consume`: other allotments which will be summed, when calcualting rest of allotments time on authorization. See examples below.

#### Examples

##### "increment", "minimum" and "no_consume_time"

```json
      "outbound_local": {
           "increment": 10,
           "minimum": 60,
           "no_consume_time": 5
       }
```
Call consumed time rounded before store it to DB.
Call with duration 40 seconds will be count as 60 seconds.
69 seconds -> 70
75 seconds -> 80
5 seconds -> 0
6 seconds -> 60

#### "group_consume"

```json
      "Class1": {
           "amount": 600,
           "group_consume": [
               "Class2"
           ]
       },
       "Class2": {
           "amount": 600,
           "group_consume": [
               "Class1"
           ]
       }
```
Here we have 2 classifiers which share same counter.
If Class1 already counsmed 400 seconds and Class2 consumed 150 seconds, next call with classifier Class2 (or Class1) will have 50 free seconds.

Little more complex example:
```json
      "Class1": {
           "amount": 600,
           "group_consume": [
               "Class2",
               "Class3"
           ]
       },
       "Class2": {
           "amount": 120,
           "group_consume": [
               "Class1"
           ]
       },
       "Class3": {
           "amount": 300,
           "group_consume": [
                "Class2"
           ]
       }
```
So if we already have counsumed calls:
Class1 - 300
Class2 - 60
Class3 - 180

As result next call wil have this free seconds:
Class1 - 60 (300 Class1 + 60 Class2 + 180 Class3 = 540, 600-540 = 60)
Class2 - 0 (60 Class2 + 300 Class1 = 360, 360 > 120)
Class3 - 60 (180 Class3 + 60 Class2 = 240, 300-240 = 60)

## Update allotments configuration for a given account

### Request

- Verb: `POST`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/limits`
- Payload:

```
{
    "data": {
        "outbound_national": {
            "amount": 3600,
            "cycle": "monthly",
            "increment": 60,
            "minimum": 60,
            "no_consume_time": 2,
            "group_consume": [
                "outbound_local"
            ]
        },
        "outbound_local": {
            "amount": 3600,
            "cycle": "monthly",
            "increment": 60,
            "minimum": 60,
            "no_consume_time": 2,
            "group_consume": [
                "outbound_national"
            ]
        }
    }
}
```

## Get consumed allotments for a given account

### Request
- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/allotments/consumed`
- Payload: None

### Response

```JSON
{
    "data": {
        "outbound_local": {
            "consumed": 120,
            "consumed_to": 63608284800,
            "consumed_from": 63605606400,
            "cycle": "monthly"
        },
        "outbound_national": {
            "consumed": 120,
            "consumed_to": 63606384000,
            "consumed_from": 63605779200,
            "cycle": "weekly"
        }
    },
    "status": "success",
}
```

## Get consumed allotments for a certain period of time

### Request
- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/allotments/consumed?created_from={{TIMESTAMP}}&created_to={{TIMESTAMP}}`
- Payload: None

`{{TIMESTAMP}}` - Gregorian epoch seconds.

### Response

```JSON
{
    "data": {
        "outbound_local": {
            "consumed": 180,
            "consumed_to": 63607728001,
            "consumed_from": 63605046001,
            "cycle": "manual"
        },
        "outbound_national": {
            "consumed": 120,
            "consumed_to": 63607728001,
            "consumed_from": 63605046001,
            "cycle": "manual"
        }
    },
    "status": "success",
}
```

## Get consumed allotments at certain time

### Request
- Verb: `GET`
- Url: `/v2/accounts/{{ACCOUNT_ID}}/allotments/consumed?created_from={{TIMESTAMP}}`
OR
`/v2/accounts/{{ACCOUNT_ID}}/allotments/consumed?created_to={{TIMESTAMP}}`
- Payload: None

`{{TIMESTAMP}}` - Gregorian epoch seconds.

```ASCII

                                {{TIMESTAMP}}
                                     ||
----+--------------------+-----------||-------+--------------------+--------
    | week3              | week4     ||       | week5              | week6
----+------------+-------+-----------||-------+--------------------+--------
 month1          | month2            ||
-----------------+-------------------||-------------------------------------

```

### Response

```JSON
{
    "data": {
        "outbound_local": {
            "consumed": 180,
            "consumed_to": month2_end_timestamp,
            "consumed_from": month2_start_timestamp,
            "cycle": "monthly"
        },
        "outbound_national": {
            "consumed": 60,
            "consumed_to": week4_end_timestamp,
            "consumed_from": week4_start_timestamp,
            "cycle": "weekly"
        }
    },
    "status": "success",
}
```
