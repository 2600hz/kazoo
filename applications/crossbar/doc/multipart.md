/*
Section: Crossbar
Title: Multi-part Request
Language: en-US
Version: 3.20
*/

# Crossbar Multi-part Request

Some APIs support sending a multipart request, such as outgoing faxes.

## Outgoing Faxes

With multipart you can create an outgoing fax request and upload the document to fax (eg: a pdf file) at the same time.

### Create a JSON file for the outgoing fax options

    {"data": {
        "retries": 3,
        "from_name": "Fax Sender",
        "from_number": "{FROM_NUMBER}",
        "to_name": "Fax Recipient",
        "to_number": "{TO_NUMBER}",
        "fax_identity_number": "{ID_NUMBER}",
        "fax_identity_name": "Fax Header"
        }
    }

### Execute the cURL request

    curl -v -X PUT -i \
        -H 'X-Auth-Token: {AUTH_TOKEN}' \
        -H "Content-Type: multipart/mixed" \
        -F "content=@{FILE.json}; type=application/json" \
        -F "content=@{FILE.pdf}; type=application/pdf" \
        http://{SERVER}/v2/accounts/{ACCOUNT_ID}/faxes/outgoing
