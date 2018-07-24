### Controlling SIP headers on the INVITE to/from a device

You can control SIP headers on the `INVITE` to the device or SIP headers on the `INVITE` as a result of a call initiated by the device, by setting `custom_sip_headers` object with the new format.

> **For compatibility reason**, the previous flat object format (basic key value) is still supported by merging its contents with the contents of `in` object of the new format. The result is set for SIP headers on the `INVITE` to a device and not on other calls.

#### Configuring custom SIP headers
To set custom SIP headers for calls to/from a device specify desire headers in `custom_sip_headers` in the device document as follow:

* New nested format(`in` will be set on calls to a device **and** `out` will be set on calls from a device):

```
"custom_sip_headers": {
    "in": {
        "X-device-header-in": "value"
    },
    "out": {
        "X-device-outbound":"value"
    }
}
```

* This also works, the old flat object and `in` will be merged and set on calls to a device  **and** `out` will be applied to calls from a device:

```
"custom_sip_headers": {
    "X-device-header":"value",
    “in”: {
        "X-device-in": "value"
    },
    "out": {
        "X-device-out": "value"
    }
}
```

- Legacy format(only applies to calls to a device):

```
"custom_sip_headers": "X-device-header": "value"
```
