/*
Section: Callflows
Title: Dynamic change caller id
Language: en-US
Version: 4.0
*/

The `dynamic_cid` callflow enables you to change the caller id (CID).

## Mandatory fields

**action** - Must be set to `manual`, `list`, or `lists`.

If undefined, will revert to historical behavior, *manual*.

## Manual action mode
### Optional fields

**interdigit_timeout** - default 2000 ms

Can only set the caller id number with this method.

You dial the new caller id on the keypad when prompted.

## List action mode
### Mandatory fields

**id** - cidlist, couchdb doc id of the document that contains the new
  callerid name and number information.

You can set the caller id number and the caller id name with this
method.

Please refer to the json sample documents at the end.

In this exmaple length is 2.

On a handset you dial `*2015149072508`

It's hooked in as a part of the feature codes.  The callflow regex looks like this:

    "patterns": [
        "^\\*2([0-9]{2,})$"
    ],

This means that `*2` is the "feature code" for this feature.

`01` is the entry in the `cidlist` document to use.  It's *length* is 2 digits.

`5149072508` becomes `+15149072508` and gets dialed as such.

Example "list" CouchDB document.  This CouchDB doc will end up being cached by Kazoo.  Make sure you flush changes..

If you were to use the length 1 example..

On a handset you dial
`*2115149072508`
or
`*215149072508`

In this case

`15149072508` or `5149072508` becomes `+15149072508` and gets dialed as such.

Sample JSON below.

Please note length needs to be 1 or 2 ONLY..

If you need more, you need to hack the code, or just use manual mode..

Length 2 example
```
{
   "_id": "cidlist",
   "_rev": "5-FyFaandfumIsmellthebloudofanEnglishman",
   "length" : 2,
	"entries": {
       "00": {
           "number": "16139999999",
           "name": "sssy co"
       },
       "01": {
           "number": "19058888888",
           "name": "bobs inc"
       }
   }
}
```

Length 1 example
```
{
   "_id": "cidlist",
   "_rev": "5-FyFaandfumIsmellthebloudofanEnglishman",
   "length" : 1,
	"entries": {
       "0": {
           "number": "16139999999",
           "name": "sssy co"
       },
       "1": {
           "number": "19058888888",
           "name": "bobs inc"
       }
   }
}
```

## Lists action mode
Almost the same as list mode but you can use account''s lists feature to configure entries:

- create list
````
http://{SERVER}/v2/accounts/{ACCOUNT_ID}/lists -d '{"data":{"name":"Dynamic CID","description":"List of new callerid name and number entries ","list_type":"dynamic_cid"}}'
````
(list_type field is optional here and used just for easier filtering dynamic_cid list among the others)

- create entries
````
http://{SERVER}/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries -d '{"data":{"cid_key":"01","cid_name":"My Office CID","cid_number":"+78124906700"}}'
````

- create callflow dynamic_cid featurecode
````
http://{SERVER}/v2/accounts/{ACCOUNT_ID}/callflows -d '{"data":{"flow":{"children":{},"data":{"action":"lists","id":"{LIST_ID}"},"module":"dynamic_cid"},"numbers":[],"patterns":["^\\*69([0-9]{2,})$"],"featurecode":{"number":"69","name":"dynamic_cid"}}}'
````

CID key length is always 2 in this mode.
