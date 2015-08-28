/*
Section: Callflows
Title: Dynamic change caller id
Language: en-US
Version: 4.0
*/

The `dynamic_cid` callflow enables you to change the caller id (CID).

## Mandatory fields

**action** - Must be set to `manual` or `list`.

If undefined, will revert to historical behavior, *manual*.


## Manual action mode
### Optional fields

**interdigit_timeout** - default 2000 ms

Can only set the caller id number with this method.

You dial the new caller id on the keypad when prompted.

## List action mode
### Mandatory fields

**id** - cidlist, couchdb doc id of the document that contains the new caller
  id name and number information.

You can set the caller id number and the caller id name with this
method.

On a handset you dial `*2015149072508`

It's hooked in as a part of the feature codes.  The callflow regex looks like this:
```
   "patterns": [
       "^\\*2([0-9]{2,})$"
	   ],
```

This means that `*2` is the "feature code" for this feature.

`01` is the entry in the `cidlist` document to use.

`5149072508` becomes `+15149072508` and gets dialed as such.

Example "list" CouchDB document.  This CouchDB doc will end up being
cached by Kazoo.  Make sure you flush changes..


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



