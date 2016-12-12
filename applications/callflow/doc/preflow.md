
# Preflow

Preflow is an option to execute a callflow before any other callflow. It is set on the account level and will apply on any call using callflow for that account.

```Javascript
"preflow": {
    "always": "CALLFLOW_ID"
}
```

The `CALLFLOW_ID` correspond to the callflow you want to call before any other.
