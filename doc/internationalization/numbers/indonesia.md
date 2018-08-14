# Indonesia Phone Numbers

Indonesia phone numbers are not in fixed length like in United States. It consists as short as 8 digits (rural areas/remote areas), extends to 12 digits (w/o country code). Mostly people will dial (to landline and few mobile networks) with leading 0, followed by area code (2-3 digits), and then the phone number (6 to 8 digits). Sometimes, if the dialed number is within the same area code, we can dial the 6-8 digit phone numbers directly w/o including area code. For GSM networks, every telco has its own code from the regulator. For example: leading digits 0811, 0812, and 0813 owned by Telco A, leading digits 0814, 0815, 0816 owned by Telco B, and so on.


## Reconcile Regex

```json
{
    "reconcile_regex": "^(?:\\+62\\d{8,})|(?:0\\d{8,})$"
}
```

Optionally, since we know E.164 is 15 max digits (including country code), it can be written as:

```json
{
    "reconcile_regex": "^(?:\\+62\\d{8,12})|(?:0\\d{8,12})$"
```


#### E.164 Converter

```json
{
    "e164_converters": {
        "^0(\\d{8,})$": {
            "prefix": "+62"
        },
        "^\\+62(\\d{8,})$": {
            "prefix": "+62"
        }
    }
}
```

And of course, optionally:

```json
{
    "e164_converters": {
        "^0(\\d{8,12})$": {
            "prefix": "+62"
        },
        "^+62(\\d{8,12})$": {
            "prefix": "+62"
        }
    }
}
```
