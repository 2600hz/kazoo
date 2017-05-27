## Number Rewrite

### About Number Rewrite

The `number_rewrite` callflow enables you to rewrite the number called by user.

### Schema

Validator for the Record Call callflow action

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`number` | The final destination number to call | `string` |   | `false`

## Callflow Json

```json
{
   "_id": "04c5fb047c8845e2a59c108a2dbeab8c",
   "_rev": "3-bb7b16b570cf38826f992ba528dc9880",
   "flow": {
       "data": {
           "number": "+1844xxxxxxx"
       },
       "module": "number_rewrite",
       "children": {
           "_": {
               "data": {
                   "caller_id_type": "external",
                   "ignore_early_media": false,
                   "outbound_flags": [
                   ],
                   "use_local_resources": true
               },
               "module": "offnet",
               "children": {
               }
           }
       }
   },
   "numbers": [
       "611"
   ],
   "patterns": [
   ],
   "contact_list": {
       "exclude": true
   }
}
```

The above callflow when called to 611 will send the users call to configured number +1844xxxxxxx
