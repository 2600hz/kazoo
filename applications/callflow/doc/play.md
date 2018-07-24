## Play

### About Play

Play media to the caller

#### Schema

Validator for the play callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`answer` | Whether to answer an unanswered call | `boolean()` |   | `false` |  
`endless_playback` | Loop the media continuously | `boolean()` | `false` | `false` |  
`id` | Media ID or URL of the media to play | `string()` |   | `false` |  
`loop_count` | How many times to loop the media | `integer()` |   | `false` |  
`terminators.[]` |   | `string()` |   | `false` |  
`terminators` | What DTMF can terminate playback of the audio | `array(string())` | `["1", "2", "3", "4", "5", "6", "7", "8", "9", "*", "0", "#"]` | `false` |  






#### Loop Count

If you want to play the media a number of times, include `loop_count` to do so.

#### Endless Playback

Endless playback is exactly that - playback of the media will not stop on the channel until the channel is hung up. The media is not interruptible.

`endless_playback=true` takes precedence over `loop_count>0` if both are included.
