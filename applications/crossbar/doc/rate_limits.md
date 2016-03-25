

The rate-limits API allows setting per-second and per-minute incoming SIP packets rate limits for devices and whole accounts (realms) that can be used at SBC level. The system level packet rate limits can also be set to protect the whole cluster.

#### Crossbar

Using Crossbar to modify rate_limits is very simple. There are only three actions:

* GET - Gets the current rate limits on the document (account or device)
* POST - Updates the rate limits on the document (account or device)
* DELETE - Removes the rate limits object from the document (account or device)

JSON object has self-describing structure. The name of the root key is "rate_limits".

The application-level system-wide configuration resides in system_config/frontier document and its syntax is equal to
the account-level documents.


 * Account-level document contains two sections:
   The "account" section sets rate limits for the entire account
   The "device" section sets rate limits for devices inside the account (if not overwritten by device-level rate limits)
   Both sections contain "registrations", "invites" and "total\_packets" keys with integer values of possible number of packets within
    "per\_minute" and "per\_second" sections

###### Example

````
{
    "data": {
        "account": {
            "per_minute": {
                "registrations": 100
                "invites": 100
                "total_packets": 1000
            },
            "per_second": {
                "registrations": 5
                "invites": 5
                "total_packets": 20
            }
        },
        "device": {
            "per_minute": {
                "registrations": 10
                "invites": 10
                "total_packets": 100
            },
            "per_second": {
                "registrations": 2
                "invites": 4
                "total_packets": 10
            }
        }
    }
}
````

 * device-level document contains is one level "higher" and contains only the "device" part
    which contains "registrations", "invites" and "total\_packets" keys with integer values of possible number of packets within
    "per\_minute" and "per\_second" sections

###### Example

````
{
    "data": {
        "per_minute": {
            "registrations": 60,
            "invites": 60,
            "total_packets": 180
        },
        "per_second": {
            "registrations": 2,
            "invites": 4,
            "total_packets": 7
        }
    }
}
````

##### Account rate limits URI

`/v1/accounts/{ACCOUNT_ID}/rate_limits`

This URI is used to manipulate the rate limits for the entire account

###### _GET_ - Fetch account rate limits:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/rate_limits

###### _POST_ - Update account rate limits:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/rate_limits -d '{"data": {"account": {"per_minute": {"total_packets": 3000},"per_second": {"total_packets": 50}},"device": {"per_minute": {"total_packets": 300},"per_second": {"total_packets": 5}}}}'

###### _DELETE_ - Remove account rate limits:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/rate_limits

##### Device rate limits URI

`/v1/accounts/{ACCOUNT_ID}/{THINGS}/{THING_ID}/rate_limits`

Here, `{THINGS}` would be "accounts" or "devices" and `{THING_ID}` would be a device or account id. Let's look at adding rate limits to a device.

###### _GET_ - Fetch device rate limits:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits

###### _POST_ - Update device rate limits:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits -d '{"data": {"per_minute": {"registrations": 60,"invites": 60,"total_packets": 180},"per_second": {"registrations": 2,"invites": 4,"total_packets": 7}}}'

###### _DELETE_ - Remove device rate limits:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/rate_limits
