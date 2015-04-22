/*
Section: Crossbar
Title: Multi-part Request
Language: en-US
*/

# Crossbar Multi-part Request

With multi-part you can create an outgoing fax request and upload the fax (pdf file) at the same time.

Ex:

JSON file

```
{
    "data": {
        "retries": 3,
        "from_name": "Fax Sender",
        "from_number": "{{FROM_NUMBER}}",
        "to_name": "Fax Recipient",
        "to_number": "{{TO_NUMBER}}",
        "fax_identity_number": "{{ID_NUMBER}}",
        "fax_identity_name": "Fax Header"
    }
}
```

Curl request

`curl -H "Content-Type: multipart/mixed" -F "content=@{{FILE}}.json; type=application/json" -F "content=@{{FILE}}.pdf; type=application/pdf" -H 'X-Auth-Token: {{TOKEN_AUTH}}' {{SERVER}}/v2/accounts/{{ACCOUNT_ID}}/faxes/outgoing -i -v -X PUT`