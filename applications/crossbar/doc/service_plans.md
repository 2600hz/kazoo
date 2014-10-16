/*
Section: Crossbar
Title: Service Plans
Language: en-US
*/

# Service Plans


## Listing your own Service Plans

You can list all your service plan in your current account by using the api `service_plans/available`.

### Retreiving all plans

#### Request

`/v2/accounts/ACCOUNT_ID/service_plans/available`

`curl -X GET -H "X-Auth-Token:e8b2e8f6eb860a561e817618a6763250"  http://SERVER_IP/v2/accounts/ACCOUNT_ID/service_plans/available`

#### Response

    {
        "page_size": 1,
        "data": [{
            "id": "some_plan_id",
            "name": "Test plan",
            "description": "Some description"
        }],
        "revision": "0eba9b78adeb509428d668e13241a95f",
        "request_id": "3b14483a1436bc4083bd230645551618",
        "status": "success",
        "auth_token": "e8b2e8f6eb860a561e817618a6763250"
    }

### Retreiving a plan

#### Request

`/v2/accounts/ACCOUNT_ID/service_plans/available/PLAN_ID`

`curl -X GET -H "X-Auth-Token:e8b2e8f6eb860a561e817618a6763250"  http://SERVER_IP/v2/accounts/ACCOUNT_ID/service_plans/available/PLAN_ID`

#### Response

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
        "revision": "1-fa6ff6201171f1f3d6c356eb28fc2644",
        "request_id": "e7c6f99f835c1713c2889a55fd65f0ba",
        "status": "success",
        "auth_token": "e8b2e8f6eb860a561e817618a6763250"
    }
