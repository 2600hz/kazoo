# Scenarios

The following are the anticipated scenarios of service changes being made and the resulting charges.

Let's assume an account tree of:

    M
    |- R1
       |- R2
          |- D3
       |- D2
    |- D1

Here we have the master account **M** with two children, **R1** (a reseller) and **D1** (a direct client). **R1**, in turn, has two children **R2** (another reseller) and **D2** (a direct client). Finally, **R2** has one child, **D3** (a direct client).

It is important to keep in mind not only which account is changing a service quantity but also *who* is making the change (an account user, reseller user, or master user).

## **M**

Since **M** is the master account, it doesn't really get a service plan applied to it and can do as it pleases. Phew, that was easy.

## **R1**

When **M** creates **R1**, it applies a service plan (or several) to the account. Let's use a super simple service plan (SSSP) that only charges $1 per device. After flagging the account as a reseller (`sup kazoo_services_maintenance make_reseller {R1_ACCOUNT_ID}`), **R1** now make sub-accounts for itself.

### **R1** makes changes

Now that **R1** exists and is a reseller, let's look at when the **R1** admin creates a device and see what should occur.

When attempting the first PUT to devices, Crossbar should return a 402 a payload similar to:
```
{
    "data": [
        {
            "items": [
                {
                    "category": "devices",
                    "item": "sip_device",
                    "quantity": 1,
                    "billable": 1,
                    "rate": 1,
                    "total": 1,
                    "changes": {
                        "type": "modified",
                        "difference": {
                            "quantity": 1
                        }
                    }
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
                    }
                }
            }
        }
    ],
    "error": "402",
    "message": "accept charges",
    "status": "error",
    "node": "zXvZ1MrlEbxaCB926LGDtg",
    "request_id":"{REQUEST_ID}",
    "auth_token":"{AUTH_TOKEN}"
}

The payload shows the details of what the change would entail (in absolute terms). If the request is resubmitted with the `accept_charges` flag, and the account is in good standing, the device is created and the services for **R1** are updated.

If another device creation would occur, the 402 response would be:

```
{
    "data": [
        {
            "items": [
                {
                    "category": "devices",
                    "item": "sip_device",
                    "quantity": 2,
                    "billable": 2,
                    "rate": 1,
                    "total": 2,
                    "changes": {
                        "type": "modified",
                        "difference": {
                            "quantity": 1
                        }
                    }
                }
            ],
            "activation_charges": [],
            "taxes": [],
            "summary": {
                "today": 0,
                "recurring": 2
            },
            "plan": {
                "devices": {
                    "sip_device": {
                        "rate": 1
                    }
                }
            }
        }
    ],
    "error": "402",
    "message": "accept charges",
    "status": "error",
    "node": "zXvZ1MrlEbxaCB926LGDtg",
    "request_id":"{REQUEST_ID}",
    "auth_token":"{AUTH_TOKEN}"
}
```

You can see that the quantity is now 2 for SIP devices.

### **M** makes changes to **R1**

Suppose **M** wants to make a change while masquerading as **R1**. What happens?

Creating device #3 we see that the request is not responded to with a 402 as before. Why? Since the auth account of the request (**M**) is a reseller, the services doc of **M** is used to process the request, and since **M** is the boss, the request is processed and the device saved.

If we get a services summary of **R1**, we see `"sip_device":3` as expected. **R1**, when synced with the bookkeeper, was billed for that third device, so admins of **M** need to be aware when making this service changes.

## **D1**

So what happens to direct accounts of the master account? Again, **M** creates **D1** and assigns the $1 device service plan. Unlike **R1**, **D1** won't be flagged as a reseller.

### **D1** makes changes

Just as when **R1** created its first device, **D1** tries to create the device and gets a 402 response with the summary of what the result would be in terms of service changes.

### **M** makes changes to **D1**

Again, as with **R1**, what **M** wants, **M** gets.

## **D2**

**R1** has signed up their first account, **D2**. This is a normal, direct-client account. No service plan has been assigned to **D2**. Let's see how service changes work!

### **D2** makes changes

When a user from **D2** creates a SIP device, the request is processed right away, since there's no service plan to process for **D2**.

### **R1** makes changes to **D2**

**R1** will be prompted to accept charges. The quantity presented will vary depending on if `"cascade":true` is on the service item.  If present the quantity will reflect the the sum of all instances of that item on the R1 account and all sub-accounts.

## **R2**

**R1** has been given reseller permissions to create their own sub accounts. Through some deal with **M**, **R1** creates a sub account **R2** and instructs **M** to mark **R2** as a reseller. Do note that we haven't assigned a service plan to this account.

### **R2** makes changes

Let's see what happens when **R2** creates its first device!

Interestingly, **R2** is not prompted to accept charges and the device is created. This seems to be a result of not having a service plan associated with **R2** and **R2** being a reseller. Since the change would not result in a charge for **R2** (according to Kazoo), the operation is successful.

### **R1** makes changes to **R2**

When **R1** tries to create a device in **R2**, **R1**'s services are used, a 402 "accept charges" is kicked back with 4 SIP devices as the quantity because **R1** has 3 devices at this point. This doesn't account for **R2**'s existing device because the service plan item for `sip_device` does not include the `"cascade":true` flag. Setting the flag would result in **R2** seeing 5 devices in their dry run synopsis.
