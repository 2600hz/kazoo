# Ratedecks

This service plan allows you to assign a ratedeck to an account.

At this time, there are no key/value pairs in the ratedeck item object.

## Example service plan

```json
{
    "_id":"plan_bulk_ratedeck",
    "pvt_type":"service_plan",
    "name":"Bulk Ratedeck Service Plan",
    "plans":{
        "ratedeck":{
            "bulk":{
            }
        }
    }
}
```
In this case the "item" is "bulk" which would correspond to a ratedeck uploaded using "bulk" as the "ratedeck_id".
