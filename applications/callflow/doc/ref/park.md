## Park

### About Park

#### Schema

Validator for the park callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | Action to take for the caller | `string('direct_park' | 'park' | 'retrieve' | 'auto')` | `park` | `false` |  
`custom_presence_id` | use configured presence_id and fallback to request | `boolean()` | `false` | `false` |  
`default_callback_timeout` | How long, in seconds, to wait before calling back the parker | `integer()` |   | `false` |  
`default_presence_type` | Type of presence to update | `string('early' | 'terminated' | 'confirmed')` |   | `false` |  
`default_ringback_timeout` | How long, in milliseconds, before ringing back | `integer()` |   | `false` |  
`presence_id` | use this presence_id | `string()` |   | `false` |  
`slot` | Static slot number to use | `string()` |   | `false` |  
`slots` | Statically define slots and their configuration | `object()` | `{}` | `false` |  



