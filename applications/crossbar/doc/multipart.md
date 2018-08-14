# Multi-part Request

Some APIs support sending a multi part request, such as outgoing faxes.

## Sending Multi-part Outgoing Faxes Request

With multi part you can create an outgoing fax request and upload the document to fax (e.g.: a PDF file) at the same time.

**Create a JSON file for the outgoing fax options**

```json
{
    "data": {
        "retries": 3,
        "from_name": "Fax Sender",
        "from_number": "{FROM_NUMBER}",
        "to_name": "Fax Recipient",
        "to_number": "{TO_NUMBER}",
        "fax_identity_number": "{ID_NUMBER}",
        "fax_identity_name": "Fax Header"
    }
}
```

**Execute the cURL request**

```shell
curl -v -X PUT -i \
    -H 'X-Auth-Token: {AUTH_TOKEN}' \
    -H "Content-Type: multipart/mixed" \
    -F "content=@{FILE.json}; type=application/json" \
    -F "content=@{FILE.pdf}; type=application/pdf" \
    http://{SERVER}/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
```
