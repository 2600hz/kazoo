/*
Section: Crossbar
Title: Service Plans
Language: en-US
Version: 3.20
*/

# Service Plans

## List your reseller service plans

This api allow you to list the service plans that you can subscribe to.

### Retrieving your service plans.

Useful for resellers.

#### Request

- Verb: `GET`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans`
- Payload: None


    `curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans`

#### Response

    {"page_size": 1,
     "data": [
         {"id": "some_plan_id",
          "name": "Reseller Test plan",
          "description": "Some description"
         }
     ],
     "status": "success",
     "auth_token": "{AUTH_TOKEN}"
    }

### Retrieving one of your service plans.

Useful for resellers.

#### Request

- Verb: `GET`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID}`
- Payload: None

    `curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID}`

#### Response

    {"data":{
        "name": "Macpie's plan",
        "description": "",
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
        "id": "plan_macpie"
     },
     "status": "success"
    }

### Adding service plan to an account.

Useful for resellers.

#### Request

- Verb: `POST`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID}`
- Payload: `{"data":{"id":"service_plan_id"}`

    `curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID} -d '{"data":{"id":"service_plan_id"}'`

### Removing service plan from an account.

Useful for resellers.

#### Request

- Verb: `DELETE`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID}`
- Payload: None

    `curl -X DELETE -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/{SERVICE_PLAN_ID}`


### Retrieving your current plan

This will retreive the service plan currenlty applied on your account.

#### Request

- Verb: `GET`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/current`
- Payload: None

    `curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/current`

#### Response

    {"data": {
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

## Listing Service Plans available to you

This api will list the services plan that can be applied to your account

### Retrieving all plans

#### Request


- Verb: `GET`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/available`
- Payload: None

    `curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/available`

#### Response

    {"page_size": 1,
     "data": [
         {"id": "some_plan_id",
          "name": "Test plan",
          "description": "Some description"
         }
     ],
     "status": "success",
     "auth_token": "{AUTH_TOKEN}"
    }

### Retrieving a plan

#### Request

- Verb: `GET`
- Url: `/v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}`
- Payload: None

    `curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}"  http://{SERVER_IP}/v2/accounts/{ACCOUNT_ID}/service_plans/available/{PLAN_ID}`

#### Response

    {"data": {
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
