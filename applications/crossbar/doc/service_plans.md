### Service_plans

#### About Service_plans

Handle the service plans you can subscribe to.

#### Schema

Describes services offered to sub-accounts

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bookkeepers` |   | `object` |   | `false`
`description` | Describes the service plan offering | `string` |   | `false`
`name` | A friendly name for the service plan | `string(1..128)` |   | `true`
`plan` | Outlines the service plan for various services | `object` |   | `true`


##### bookkeepers

The bookkeeper modules provided by Kazoo

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`braintree` |   | `object` |   | `false`
`local` |   | `object` |   | `false`



#### Retrieving your service plans.

> GET /v2/accounts/{ACCOUNT_ID}/service_plans

Useful for resellers.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

```json
{
    "page_size": 1,
    "data": [
        {
            "id": "some_plan_id",
            "name": "Reseller Test plan",
            "description": "Some description"
        }
    ],
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Adding/Removing multiple service plans on an account

> POST /v2/accounts/{ACCOUNT_ID}/service_plans

Useful for resellers.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "add": ["plan1", "plan2"],
        "delete": ["plan3"]
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

```json
{
    "data": {} //  Merge of the Service plans if any left
    "status": "success"
}
```

#### Removing service plan from an account

> DELETE /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

Useful for resellers.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

#### Retrieving one of your service plans.

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

Useful for resellers.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

```json
{
    "data": {
        "bookkeepers": {
            "braintree": {
                "devices": {
                    "sip_devices": {
                        "addon": "sip_device",
                        "discounts": {
                            "cumulative": "discount_did_us"
                        },
                        "plan": "SIP_Services"
                    }
                },
                "limits": {
                    "inbound_trunks": {
                        "addon": "inbound_trunk",
                        "plan": "SIP_Services"
                    },
                    "twoway_trunks": {
                        "addon": "twoway_trunk",
                        "plan": "SIP_Services"
                    }
                },
                "number_services": {
                    "e911": {
                        "addon": "e911",
                        "plan": "SIP_Services"
                    }
                },
                "phone_numbers": {
                    "did_us": {
                        "addon": "did_us",
                        "plan": "SIP_Services"
                    },
                    "tollfree_us": {
                        "addon": "tollfree_us",
                        "plan": "SIP_Services"
                    }
                }
            }
        },
        "description": "",
        "id": "plan_macpie",
        "name": "Macpies plan",
        "plan": {
            "devices": {
                "_all": {
                    "activation_charge": 3,
                    "as": "sip_devices",
                    "discounts": {
                        "cumulative": {
                            "maximum": 20,
                            "rate": 5
                        }
                    },
                    "exceptions": [
                        "cellphone",
                        "landline"
                    ],
                    "name": "SIP Device",
                    "rate": 5
                }
            },
            "limits": {
                "inbound_trunks": {
                    "name": "Inbound Trunk",
                    "rate": 6.99
                },
                "twoway_trunks": {
                    "name": "Two-Way Trunk",
                    "rate": 29.99
                }
            },
            "number_services": {
                "e911": {
                    "cascade": true,
                    "discounts": {
                        "single": {
                            "rate": 5
                        }
                    },
                    "name": "E911 Service",
                    "rate": 2
                },
                "inbound_cnam": {
                    "activation_charge": 1,
                    "name": "Inbound CNAM Update",
                    "rate": 2
                },
                "outbound_cnam": {
                    "activation_charge": 5,
                    "name": "Outbound CNAM Update",
                    "rate": 1
                },
                "port": {
                    "activation_charge": 10,
                    "name": "Port Request"
                }
            },
            "phone_numbers": {
                "did_us": {
                    "activation_charge": 3,
                    "cascade": true,
                    "name": "US DID",
                    "rate": 2
                },
                "tollfree_us": {
                    "cascade": true,
                    "name": "US Tollfree",
                    "rate": 4.99
                }
            },
            "users": {
                "_all": {
                    "activation_charge": 3,
                    "as": "user",
                    "cascade": true,
                    "exceptions": [],
                    "name": "User",
                    "rate": 5
                }
            }
        }
    },
    "status": "success"
}
```

#### Adding service plan to an account.

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"id":"{PLAN_ID}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{PLAN_ID}
```

```json
{
    "data": {...}
    "status": "success"
}
```

#### Override a plan

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/override

**Must be super duper admin**

Note: `_all` override payload.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "overrides": {
            "{PLAN_ID}": {
                "whitelabel": {
                    "_all": {
                        "activation_charge": 700
                    }
                }
            }
        }
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/override
```

```json
{
    "data": {
        "whitelabel": {
            "_all": {
                "name": "Whitelabel",
                "as": "whitelabel",
                "exceptions": [],
                "activation_charge": 700,
                "cascade": true,
                "rate": 300
            }
        }
    },
    "status": "success"
}
```

#### Retrieving your current plan

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/current

This will retreive the service plan currenlty applied on your account.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/current
```

```json
{
    "data": {
        "account_quantities": {
            "number_services": {},
            "phone_numbers": {
                "did_us": 4
            },
            "devices": {
                "sip_device": 1,
                "softphone": 2
            },
            "limits": {
                "twoway_trunks": 10,
                "inbound_trunks": 10
            },
            "users": {
                "admin": 1,
                "user": 1
            },
            "ips": {
                "dedicated": 0
            }
        },
        "cascade_quantities": {},
        "plans": {
            "plan_dedicated_install": {
                "account_id": "a0f3b6f2c5c0c95240993acd1bd6e762"
            }
        },
        "billing_id": "1760753c8d022d650418fbbe6a1a10e0",
        "reseller": false,
        "reseller_id": "a0f3b6f2c5c0c95240993acd1bd6e762",
        "dirty": false,
        "in_good_standing": true,
        "items": {
            "number_services": {
                "port": {
                    "category": "number_services",
                    "item": "port",
                    "quantity": 0,
                    "single_discount": false,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                },
                "outbound_cnam": {
                    "category": "number_services",
                    "item": "outbound_cnam",
                    "quantity": 0,
                    "rate": 1.0,
                    "single_discount": false,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                },
                "inbound_cnam": {
                    "category": "number_services",
                    "item": "inbound_cnam",
                    "quantity": 0,
                    "rate": 2.0,
                    "single_discount": false,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                },
                "e911": {
                    "category": "number_services",
                    "item": "e911",
                    "quantity": 0,
                    "rate": 2.0,
                    "single_discount": false,
                    "single_discount_rate": 5.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                }
            },
            "devices": {
                "sip_devices": {
                    "category": "devices",
                    "item": "sip_devices",
                    "quantity": 3,
                    "rate": 5.0,
                    "single_discount": true,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 3,
                    "cumulative_discount_rate": 5.0
                }
            },
            "phone_numbers": {
                "tollfree_us": {
                    "category": "phone_numbers",
                    "item": "tollfree_us",
                    "quantity": 0,
                    "rate": 4.9900000000000002132,
                    "single_discount": false,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                },
                "did_us": {
                    "category": "phone_numbers",
                    "item": "did_us",
                    "quantity": 4,
                    "rate": 2.0,
                    "single_discount": true,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                }
            },
            "users": {
                "user": {
                    "category": "users",
                    "item": "user",
                    "quantity": 2,
                    "rate": 5.0,
                    "single_discount": true,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                }
            },
            "limits": {
                "twoway_trunks": {
                    "category": "limits",
                    "item": "twoway_trunks",
                    "quantity": 10,
                    "rate": 29.989999999999998437,
                    "single_discount": true,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                },
                "inbound_trunks": {
                    "category": "limits",
                    "item": "inbound_trunks",
                    "quantity": 10,
                    "rate": 6.9900000000000002132,
                    "single_discount": true,
                    "single_discount_rate": 0.0,
                    "cumulative_discount": 0,
                    "cumulative_discount_rate": 0.0
                }
            }
        }
    },
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

#### Listing Service Plans available to you

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available

This api will list the services plan that can be applied to your account

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available
```

```json
{
    "page_size": 1,
    "data": [
        {
            "id": "some_plan_id",
            "name": "Test plan",
            "description": "Some description"
        }
    ],
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```


#### Retrieving a plan

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}
```

```json
{
    "data": {
        "name": "Test plan",
        "description": "Some description",
        "plan": {
            "phone_numbers": {
                "did_us": {
                    "name": "US DID",
                    "rate": 2,
                    "activation_charge": 3,
                    "cascade": true
                },
                "tollfree_us": {
                    "name": "US Tollfree",
                    "rate": 4.9900000000000002132,
                    "cascade": true
                }
            },
            "number_services": {
                "outbound_cnam": {
                    "name": "Outbound CNAM Update",
                    "activation_charge": 5,
                    "rate": 1
                },
                "inbound_cnam": {
                    "rate": 2,
                    "name": "Inbound CNAM Update",
                    "activation_charge": 1
                },
                "port": {
                    "name": "Port Request",
                    "activation_charge": 10
                },
                "e911": {
                    "name": "E911 Service",
                    "rate": 2,
                    "cascade": true,
                    "discounts": {
                        "single": {
                            "rate": 5
                        }
                    }
                }
            },
            "limits": {
                "twoway_trunks": {
                    "name": "Two-Way Trunk",
                    "rate": 29.989999999999998437
                },
                "inbound_trunks": {
                    "name": "Inbound Trunk",
                    "rate": 6.9900000000000002132
                }
            },
            "devices": {
                "_all": {
                    "name": "SIP Device",
                    "as": "sip_devices",
                    "exceptions": ["cellphone", "landline"],
                    "activation_charge": 3,
                    "rate": 5,
                    "discounts": {
                        "cumulative": {
                            "maximum": 20,
                            "rate": 5
                        }
                    }
                }
            },
            "users": {
                "_all": {
                    "name": "User",
                    "as": "user",
                    "exceptions": [],
                    "activation_charge": 3,
                    "cascade": true,
                    "rate": 5
                }
            }
        },
        "bookkeepers": {
            "braintree": {
                "phone_numbers": {
                    "did_us": {
                        "plan": "SIP_Services",
                        "addon": "did_us"
                    },
                    "tollfree_us": {
                        "plan": "SIP_Services",
                        "addon": "tollfree_us"
                    }
                },
                "number_services": {
                    "e911": {
                        "plan": "SIP_Services",
                        "addon": "e911"
                    }
                },
                "limits": {
                    "twoway_trunks": {
                        "plan": "SIP_Services",
                        "addon": "twoway_trunk"
                    },
                    "inbound_trunks": {
                        "plan": "SIP_Services",
                        "addon": "inbound_trunk"
                    }
                },
                "devices": {
                    "sip_devices": {
                        "plan": "SIP_Services",
                        "addon": "sip_device",
                        "discounts": {
                            "cumulative": "discount_did_us"
                        }
                    }
                }
            }
        },
        "id": "some_plan_id"
     },
     "status": "success",
     "auth_token": "{AUTH_TOKEN}"
}
```
