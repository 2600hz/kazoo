
# Ratedeck name

This service plan item allows you to assign some ratedeck name to account. This name will be used by HotOrNot application to select appropriate rate.

This item is similar to other service plan items, with one exceptions - quantity of this item will always be equal 1.

Example service plan:
```JSON
{
    "pvt_type": "service_plan",
    "name": "Ratedeck 1",
    "description": "Service plan with ratedeck",
    "plan": {
        "ratedeck_name": {
            "ratedeck_id_1": {
                "name": "Ratedeck 1"
            }
        }
    }
}
```

**Note: each account should have only one service plan with only one `ratedeck_name` item in it**.
When you will try add another service plan with `ratedeck_name` item, on reconcilation there will be generated system alert notification (and error message in log) about this situation. `ratedeck_name` item will stay unchanged until you remove one of conflicting service plans.