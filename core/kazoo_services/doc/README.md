# In the beginning

There was a service\_plan doc. It defined a simple service: charge $1 per sip device. And lo, it looked like this:
```
{
    "_id": "plan_simple",
    "_rev": "1-revision",
    "name": "Super Simple Service Plan",
    "plan": {
        "devices": {
            "sip_device": {
                "rate": 1
            }
        }
    },
    "pvt_type": "service_plan"
}
```

And the admins rejoiced, for they could now bill their accounts for SIP devices.

## Plan

The `plan` object of a service plan is comprised of item objects that are grouped by category.  Each category/item that occurs on more than one assigned service plan will be merged (described in more detail later).

The available parameters for a plan category/item are:
* `activation_charge` : A charge to apply when a new instance of the category/item is created in the account.
* `minimum` : The minimum quantity of the item, this value is used as the quantity until the account's actually quantity exceeds it.
* `flat_rates` : The values in this object dictate a fixed charge at a quantity, this is different than `rate` or `rates` which are multiplied against the current quantity.  If this is set this property has precedence over the other rate parameters.
* `rates` : The values in this object dictate the quantity thresholds of a tiered rate.
* `rate` : This value is the rate to apply when there are no `flat_rates` or `rates` for the given quantity.
* `cascade` : If set to 'true' the quantities of this item include the quantities for the account and all sub-account's the plan is applied to.
* `name` : An optional friendly name to use, if not present the category/item is used.
* `discounts.single.rates` : The values in this object dictate the quantity thresholds for a tiered discount to apply if there is one or more items.
* `discounts.single.rate` : The discount to apply if the billable quantity is greater than one and no `discounts.single.rates` match.
* `discounts.cumulative.rates` : The values in this object dictate the quantity thresholds for a tiered discount to apply to each billable quantity (up to `discounts.cumulative.maximum`).
* `discounts.cumulative.rate` : The discount if no `discounts.cumulative.rates` match applied to each billable quantity (up to `discounts.cumulative.maximum`).
* `discounts.cumulative.maximum` : The maximum quantity of billable quantities to apply the cumulative discount to.

A special reserved item is `_all`.  When used in a category/item this keyword results in quantities as a sum of the category.  For example, `devices._all` would be applied to all enabled device objects in an account regardless of the documents `device_type` property.  When using the `_all` keyword additional properties are available.
* `as` : The name of the item to masquerade as for the bookkeeper.  For example, it might be useful to set `as` to 'sip_device' for `devices._all`.
* `exceptions` : When summing a category, this array of items should be excluded.  For example, to not consider the `device_type` 'softphone' in `devices._all` set the parameter to an array containing that device type.

## Quantities

If a category/item name from the plan match a quantity category/item name the plan's parameters will be applied to those quantities to generate a billable item on a quote/invoice.

Kazoo will automatically provide quantities for the following category/item:
* `users.{PRIV_LEVEL}` : For example, `users.admin` is the quantity of enabled user documents with the property `priv_level` set to 'admin'.  If the document has no `priv_level` property it will default to 'user'.
* `devices.{DEVICE_TYPE}` : For example, `devices.softphone` is the quantity of enabled device documents with the property `device_type` set to 'softphone'.  If the device document has no `device_type` property it will default to 'sip_device'.
* `limits.{TRUNK_TYPE}` : For example, `limits.twoway_trunks` is quantity of trunks that are available for either inbound or outbound flat rate calls.  At this time time there are only three trunk types, 'twoway_trunks', 'inbound_trunks' and 'outbound_trunks'.
* `branding.whitelabel` : This is the quantity of whitelabeled realms.
* `ips.dedicated` : This is the quantity of assigned dedicated IPs
* `user_apps.{APP_NAME}` : For example, `user_apps.callflows` is the quantity of users that have the Monster UI Callflow application enabled/installed.
* `account_apps.{APP_NAME}` : For example, `account_apps.callflows` will have a quantity of 1 for all accounts that have have the Monster UI application enabled/installed regardless of how many users have access to it.
* `phone_numbers.{CLASSIFICATION}` : This is the quantity of phone numbers assigned to an account by classification as configured on `system_config/number_manager.classifiers`. For example, `phone_numbers.did_us` is the quantity of phone numbers that match the US DID regular expression.
* `number_carriers.{CARRIER_NAME}` : This is the quantity of phone number assigned to an account by the carrier name on the phone number document.  For example, `number_carriers.local` is the quantity of numbers managed by the account admin (typically BYOC).
* `number_services.{SERVICE}` : This is the quantity of phone number features configured on numbers assigned to an account.  For example, `number_services.e911` is the quantity of numbers that have E911 (US emergency services) enabled.
* `voicemails.vmbox` : This is the quantity of enabled voicemail boxes in an account.
* `faxes.faxbox` : This is the quantity of enabled fax boxes in an account.
* `conferences.conference` : This is the quantity of enabled conferences in an account.

Manual quantities can be added and matched to a service plan category/item (see below).

## Applications

A service plan can optionally control the Monster UI applications that are available in the Monster UI application exchange.  If all service plan(s) assigned to an account do *not* dictate a list of exposed applications then all applications are available to the account administrator.  However, if any service plan(s) assigned to an account dictate one or more applications then the unique list of enabled applications across all assigned service plans are the only applications available to the account administrator.

An example of a service plan that charges $1 per sip device as well as restricting access to only the Smart PBX Monster UI application:
```
{
    "_id": "plan_simple",
    "_rev": "1-revision",
    "name": "Super Simple Service Plan",
    "plan": {
        "devices": {
            "sip_device": {
                "rate": 1
            }
        }
    },
    "applications": {
        "{SMART_PBX_APP_ID}": {
            "vendor_id": "{RESELLER_ID}",
            "name": "voip"
        }
    },
    "pvt_type": "service_plan"
}
```

Note that the `vendor_id` is assumed to be the master account id and the property `enabled` defaults to `true` if either are not present.

# Ratedecks

This service plan(s) allows you to assign a ratedeck to an account.  At this time only a single ratedeck can be assigned to an account so all services plans will be merged (see below) to a single plan and the resulting ratedeck properties extracted.

An example of a service plan that charges $1 per sip device, restricted access to only the Smart PBX Monster UI application and set the ratedeck to correspond to a ratedeck uploaded using 'bulk' as the `ratedeck_id`:
```
{
    "_id": "plan_simple",
    "_rev": "1-revision",
    "name": "Super Simple Service Plan",
    "plan": {
        "devices": {
            "sip_device": {
                "rate": 1
            }
        }
    },
    "applications": {
        "{SMART_PBX_APP_ID}": {
            "vendor_id": "{RESELLER_ID}",
            "name": "voip"
        }
    },
    "ratedeck": {
        "id": "bulk"
    },
    "pvt_type": "service_plan"
}
```

## Overrides

Overrides can be used to modify properties of the assigned service plan(s) on a per-account basis and in implementation are simply merged onto the service plan document or the merged group plan for account wide overrides (see below for more details on the merge process).

To fetch the overrides that apply to all service plans the following API can be used:

`GET /v2/accounts/{ACCOUNT_ID}/services/overrides`

Using the same API a POST can be used to change the overrides.  For example the following request payload overrides the bookkeeper type, exposes the Monster UI application 'Callflows', disables the Monster UI application 'Smart PBX' and sets the device price for all service plans:

`POST /v2/accounts/{ACCOUNT_ID}/services/overrides`
```
{
    "data": {
        "bookkeeper": {
            "type": "example"
        },
        "applications": {
            "{CALLFLOWS_APP_ID}": {
                "vendor_id": "{MASTER_ACCOUNT_ID}",
                "name": "callflows"
            },
            "{SMART_PBX_APP_ID}": {
                "enabled" false,
                "vendor_id": "{RESELLER_ID}",
                "name": "voip"
            }
        },
        "plan": {
            "devices": {
                "sip_device": {
                    "rate": 12
                }
            }
        }
    }
}
```

To determine what plan properties can be modified in the overrides the following API can be invoked:

`GET /v2/accounts/{ACCOUNT_ID}/services/editable`

## Merge

When multiple service plans are assigned to an account they must be merged together resulting in an aggregate plan, this process is as follows:

* Each assigned plan is individually recursively merged with any plan specific overrides, overrides have priority
* The plans are grouped by bookkeeper (bookkeeper groups)
* Each bookkeeper group is further grouped by `merge.strategy` (strategy groups)
* The strategy group plans are sorted by `merge.priority` (larger integers are higher priority) then merged into a single plan for that strategy group
* The strategy groups (now a single plan) for each bookkeeper group are merged into a single bookkeeper group plan
    * This is a recursive merge meaning the resulting single bookkeeper group plan can have properties from each strategy group
    * The strategies are prioritized by `system_config\services.merge_strategy_priority` (larger integers are higher priority)
* Any global overrides are recursively merged into each bookkeeper group, again overrides have priority

The property `merge.strategy` controls the merge type.  There are 3 options:
* 'simple': If not specified this is the default.  The highest priority plan that defines a particular category/item is used.  For example, if multiple plans define devices.sip_device only the properties of the highest priority plan with this definition is used.
* 'recursive': The highest priority parameter of a category/item is used.  For example, if one plan defines a rate as well as a discount and another defines just a rate the result would be a devices.sip_device with a rate from the highest priority plan and a discount from the only plan defining that parameter.
* 'cumulative': In this strategy `merge.priority` is only used to resolve conflicts for `activation_charge`, `flat_rates`, `rate`, `as`, `name`, `discounts.single.rate` and `discounts.cumulative.rate`.  The properties `minimum`, and `discounts.cumulative.maximum` are summed.  The properties `rates`, `discounts.single.rates` and `discounts.cumulative.rates` are merged.  The property `exceptions` is the unique list from all plans with this strategy. Finally, the property `cascade` will be `true` if any plan in this strategy has the property set as `true`.

## Quotes

Prior to account creation it might be useful to get quotes based on one or more service plans.  Since each service plan is assigned to a bookkeeper, if the provided service plans belong to more than one bookkeeper a quote will be returned for each bookkeeper.

An example request payload to create quote(s) using the 'plan_complex' with overrides and 'plan_simple' would be:

`POST /v2/services/quote`
```
{
    "data": {
        "plans": [
            {
                "id": "plan_simple",
                "overrides": {
                    "plan": {
                        "devices": {
                            "sip_device": {
                                "rate": 0.25
                            }
                        }
                    }
                }
            },
            "plan_complex"
        ]
    }
}
```

If you would like the quote to be generated with the current quantities of the account issue the request with the same payload but use the API:

`POST /v2/accounts/{ACCOUNT_ID}/services/quote`

The result of either of these request is identical to a summary request (see below), however the account and services are unmodified.

## Invoices

Invoices are generated by merging the services plan(s) assigned to the account and combining the results with the current quantities.  Like quotes, since each service plan is assigned to a bookkeeper it is possible for an account to have more than one invoice.

To view the current invoice(s) for an account issue a summary request (see below).

Additionally, the invoice(s) will be returned to any request that modifies a billable quantity as a '402 Payment Required' annotated with the differences between the current and proposed invoice(s).  See accepting charges below for more information.

## Summary

To summarize the services of an account issue a request to the following API:

`GET /v2/accounts/{ACCOUNT_ID}/services/summary`

The result payload will provide:
* Assigned plans with any configured overrides
* The current invoices including:
** The merged plan used to generate the items
** Any activation charges (only populated for requests that require charge acceptance)
** Taxes (not currently populated)
** A summary of the recurring and activation charges the invoice represents
** If the invoice is associated with a bookkeeper, that association details
* The quantities of billable category/items

```
{
    "data": {
        "plans": {
            "plan_simple": {
                "vendor_id": "{RESELLER_ID}",
                "overrides": {}
            },
            "plan_complex": {
                "vendor_id": "{RESELLER_ID}",
                "overrides": {}
            }
        },
        "invoices": [
            {
                "items": [
                    {
                        "category": "devices",
                        "item": "sip_device",
                        "quantity": 1,
                        "billable": 1,
                        "rate": 1,
                        "total": 1
                    },
                    {
                        "category": "devices",
                        "item": "_all",
                        "quantity": 1,
                        "billable": 1,
                        "rate": 0,
                        "total": 0
                    }
                ],
                "activation_charges": [],
                "taxes": [],
                "summary": {
                    "today": 0,
                    "recurring": 1
                },
                "plan": {
                    "devices": {
                        "sip_device": {
                            "rate": 1
                        },
                        "_all": {
                            "discounts": {
                                "cumulative": {
                                    "maximum": 1
                                }
                            }
                        }
                    }
                },
                "bookkeeper": {
                    "id": "{BOOKKEEPER_ID}",
                    "vendor_id": "{RESELLER_ID}",
                    "type": "{BOOKKEEPER_TYPE}"
                }
            },
            {
                "items": [
                    {
                        "category": "phone_numbers",
                        "item": "did_us",
                        "name": "US DID Phone Number",
                        "quantity": 14,
                        "billable": 14,
                        "rate": 1,
                        "total": 14
                    },
                    {
                        "category": "phone_numbers",
                        "item": "tollfree_us",
                        "name": "US Tollfree Phone Number",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 4.99,
                        "total": 0
                    },
                    {
                        "category": "phone_numbers",
                        "item": "international",
                        "name": "International Phone Number",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 4.99,
                        "total": 0
                    },
                    {
                        "category": "number_services",
                        "item": "e911",
                        "name": "E911 Service",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 2,
                        "total": 0
                    },
                    {
                        "category": "limits",
                        "item": "twoway_trunks",
                        "name": "Two-Way Trunk",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 24.99,
                        "total": 0
                    },
                    {
                        "category": "limits",
                        "item": "inbound_trunks",
                        "name": "Inbound Trunk",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 6.99,
                        "total": 0
                    },
                    {
                        "category": "limits",
                        "item": "outbound_trunks",
                        "name": "Outbound Trunk",
                        "quantity": 0,
                        "billable": 0,
                        "rate": 21.99,
                        "total": 0
                    },
                    {
                        "category": "users",
                        "item": "user",
                        "name": "User",
                        "quantity": 8,
                        "billable": 8,
                        "rate": 18.99,
                        "total": 151.92
                    }
                ],
                "activation_charges": [],
                "taxes": [],
                "summary": {
                    "today": 0,
                    "recurring": 165.92
                },
                "plan": {
                    "phone_numbers": {
                        "did_us": {
                            "name": "US DID Phone Number",
                            "rate": 1,
                            "cascade": true
                        },
                        "tollfree_us": {
                            "name": "US Tollfree Phone Number",
                            "rate": 4.99,
                            "cascade": true
                        },
                        "international": {
                            "name": "International Phone Number",
                            "rate": 4.99,
                            "cascade": true
                        }
                    },
                    "number_services": {
                        "e911": {
                            "name": "E911 Service",
                            "rate": 2,
                            "cascade": true
                        }
                    },
                    "limits": {
                        "twoway_trunks": {
                            "name": "Two-Way Trunk",
                            "rate": 24.99,
                            "cascade": false
                        },
                        "inbound_trunks": {
                            "name": "Inbound Trunk",
                            "rate": 6.99,
                            "cascade": false
                        },
                        "outbound_trunks": {
                            "name": "Outbound Trunk",
                            "rate": 21.99,
                            "cascade": false
                        }
                    },
                    "users": {
                        "_all": {
                            "as": "user",
                            "name": "User",
                            "rate": 18.99,
                            "cascade": true
                        }
                    }
                }
            }
        ],
        "quantities": {
            "account": {
                "account_apps": {
                    "accounts": 1,
                    "callflows": 1,
                    "fax": 1,
                    "numbers": 1,
                    "pbxs": 1,
                    "voicemails": 1,
                    "voip": 1
                },
                "branding": {
                    "whitelabel": 1
                },
                "devices": {
                    "sip_device": 1
                },
                "faxes": {
                    "mailbox": 1
                },
                "number_carriers": {
                    "knm_inventory": 4
                },
                "phone_numbers": {
                    "did_us": 4
                },
                "user_apps": {
                    "accounts": 2,
                    "callflows": 2,
                    "fax": 2,
                    "numbers": 2,
                    "pbxs": 2,
                    "voicemails": 2,
                    "voip": 2
                },
                "users": {
                    "admin": 1,
                    "user": 4
                },
                "voicemails": {
                    "mailbox": 3
                }
            },
            "cascade": {
                "branding": {
                    "whitelabel": 1
                },
                "conferences": {
                    "conference": 1
                },
                "devices": {
                    "sip_device": 3
                },
                "ips": {
                    "dedicated": 2
                },
                "number_carriers": {
                    "knm_inventory": 5,
                    "knm_local": 5
                },
                "number_services": {
                    "inbound_cnam": 2,
                    "local": 5,
                    "outbound_cnam": 1
                },
                "phone_numbers": {
                    "did_us": 10
                },
                "users": {
                    "admin": 3,
                    "user": 3
                },
                "voicemails": {
                    "mailbox": 6
                }
            },
            "manual": {}
        },
        "reseller": {
            "id": "{RESELLER_ID}",
            "is_reseller": true
        },
        "ratedeck": {}
    }
}
```

# The Account Is Born

An account is created! As part of account creation, an initial full reconcile is run to create the initial services document for the account (found in the services database with the account's ID as the doc ID).

The initial services doc looks something like this:
```
{
    "_id": "{ACCOUNT_ID}",
    "quantities": {
        "manual": {},
        "account": {},
        "cascade": {}
    },
    "pvt_type": "service"
}
```
We can see that the initial quantities of "billable" things have been initialized.

# Service Plan(s) Assignments

Once the account is created one or more service plans can be assigned.  To list the services plan(s) that a reseller has created and can be applied to the account the following API is available:

`GET /v2/accounts/{ACCOUNT_ID}/services/available`
```
{
    "page_size": 2,
    "data": [
        {
            "id": "plan_simple",
            "name": "Super Simple Service Plan",
            "description": "A simple example plan that only charges for devices.",
            "category": "Base Plan"
        },
        {
            "id": "plan_complex",
            "name": "More Complex Service Plan",
            "description": "A more complex plan that charges for several services",
            "category": "Base Plan"
        }
    ]
}
```

There are two APIs that can be used to assign plans to the account, the first is useful for individual updates:

`POST /v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}`

The payload of this request can set the overrides, but if no overrides are required this will just be an empty data object.  An example of the request payload to override the device price on this plan for this account only might be:
```
{
    "data": {
        "overrides": {
            "plan": {
                "devices": {
                    "sip_device": {
                        "rate": 1.5
                    }
                }
            }
        }
    }
}
```

The other option would be to perform a 'bulk' change to the assignments via:

`POST /v2/accounts/{ACCOUNT_ID}/services`

The payload of this request is more complicated, and can assign, unassign as well as set global or plan level override:
```
{
    "data": {
        "add": [
            {
                "id": "plan_complex",
                "overrides": {}
            },
            "plan_simple"
        ],
        "delete": [
            "plan_deprecated"
        ],
        "overrides": {
            "plan": {
                "devices": {
                    "sip_device": {
                        "rate": 0.25
                    }
                }
            }
        }
    }
}
```

To see what service plan(s) are current assigned to an account the API `GET /v2/accounts/{ACCOUNT_ID}/services` can be requested and return a payload such as:
```
{
    "data": {
        "plan_simple": {
            "vendor_id": "{RESELLER_ID}",
            "overrides": {
                "plan": {
                    "devices": {
                        "sip_device": {
                            "rate": 1.5
                        }
                    }
                }
            }
        },
        "plan_complex": {
            "vendor_id": "{RESELLER_ID}",
            "overrides": {}
        }
    }
}
```

We can also see the plans applied to this account on the services document in the database:
```
{
    "_id": "{ACCOUNT_ID}",
    "quantities": {
        "manual": {},
        "account": {},
        "cascade": {}
    },
    "plans": {
        "plan_complex": {
            "vendor_id": "{RESELLER_ID}",
            "overrides": {}
        },
        "plan_simple": {
            "vendor_id": "{RESELLER_ID}",
            "overrides": {
                "plan": {
                    "devices": {
                        "sip_device": {
                            "rate": 1.5
                        }
                    }
                }
            }
        }
    },
    "overrides": {
        "plan": {
            "devices": {
                "sip_device": {
                    "rate": 0.25
                }
            }
        }
    },
    "pvt_type": "service"
}
```

We could create all sorts of different plans and apply multiple to this account.

# The Account Grows

The admin for the account logs in and wants to create a device. Let's see what happens!

When the `PUT /v2/accounts/{ACCOUNT_ID}/devices` comes in and is validated, it is time to actual try to create the device. However, Kazoo needs to find out if this would cause a billable event and warn the client making the request. A `dry_run` request is made to see what the impact of the change would be by providing kz_services with the version of the current document (if any) and the proposed change to the document.  The Erlang equivalent of the account's view 'services/quantities' is invoked to determine how the quantities might be impacted.  The service plan(s) assigned to the account are then fetched and any invoice(s) generated from both the current quantities as well as the proposed quantities.  The current and proposed invoice(s) are then compared to determine if there are any differences.

If the invoice(s) were modified by the request Crossbar now knows that if the action is performed, the account will be charged. Crossbar then checks to see if the request has explicitly indicated it will accept the charges (by including `"accept_charges":true` on the request payload). If the request is not accepting charges, a 402 response is sent back with the updated invoice(s). The client can then decide if they'd like to accept the charges and resubmit the `PUT` with `"accept_charges":true` set.

# Accepting Charges

When resubmitted, the same dry_run process is executed, the request is found to accept charges, and the `PUT` is executed (in this case, the device is saved to the database). Once the update has been completed the kz_services module updates the quantities on the services document, re-generates any invoice(s) as dictated by the service plan(s) and provides them to the appropriate bookkeeper application(s).

This also creates an audit log entry to be added to the impacted account's MODb. This audit log tracks who did what to cause a billing event to occur as well as the result of the bookkeeper.

Once the account has been processed the process is repeated for each reseller in the hierarchy to support cascade quantities and subsequent billing.

# Manual Quantities

Manual quantities can be fetch using the following API:

`GET /v2/accounts/{ACCOUNT_ID}/services/manual`

Using a `POST` request the manual quantities for an account can be set and a `PATCH` will only modify the provided properties.  An example request payload to modify the manual quantity of `spacely_space.sprockets`:

```
{
    "data": {
        "spacely_space": {
            "sprockets": 1
        }
    }
}
```

If the assigned service plan(s) have a plan object for this category/item then it will be incorporated into the quote/invoice.

If there is a conflict between an account or cascade quantity and the manual quantities, the manual quantities have priority.

# Audit Logs

Every billable change results in an audit log entry added to the impacted account's MODb. This audit log tracks who did what to cause a billing event to occur as well as the result of the bookkeeper.  These are available via the API `GET /v2/accounts/{ACCOUNT_ID}/services/audit` for a summary or `GET /v2/accounts/{ACCOUNT_ID}/services/audit/{AUDIT_ID}` for the full details.

The master account won't save audit logs unless configured to do so. In `system_config/services`, toggle the flag `should_save_master_audit_logs` to 'true'. Otherwise only the reseller tree minus the master account will have audit logs saved.

# Reconciliation

Reconciliation is the process of ensuring that the quantities on the services document accurately reflect the actual quantities.  When reconciling an account, the view 'services/quantity' is requested from the account database which will return the quantities of all billable objects and/or properties for that account.  Using the view's reduce function these are grouped by billing category and item name then summed.  Further, the view 'services/cascade_quantities' is requested from the services database.  This view has a reduce function that groups by parent account id, billing category and item names then also sums the results.  These quantities then overwrite the respective quantities objects ('account' and 'cascade') on the services doc in the services database.

Once reconciliation has finished, the services doc's `quantities` should reflect the actual quantities of the account for that service category/item.

A reconciliation can be triggered via `sup kazoo_services_maintenance reconcile {ACCOUNT_ID}` or the API as `POST /v2/accounts/{ACCOUNT_ID}/services/reconciliation`

# Synchronization

Synchronization is the process of ensuring that the bookkeepers accurately reflect the invoice(s) generated from the assigned service plan(s).  When synchronizing an account the current quantities are fetched from the services document as well as the service plan(s) assigned to the account.  These are combined to generated the account's invoice(s) and grouped by the associated bookkeeper.  Each bookkeeper application is then issued an AMQP request with the invoice and expected to preform the appropriate action to ensure that the bookkeeper properly reflects the invoice.

A synchronization can be triggered via `sup kazoo_services_maintenance sync {ACCOUNT_ID}` or the API as `POST /v2/accounts/{ACCOUNT_ID}/services/synchronization`
