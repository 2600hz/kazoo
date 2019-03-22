## Conference

### About Conference

#### Schema

Validator for the Conference callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`config` | Build an ad-hoc conference using the conferences JSON schema | `object()` |   | `false` |  
`id` | Kazoo ID of the conference | `string(32)` |   | `false` |  
`moderator` | Is the caller entering the conference as a moderator | `boolean()` |   | `false` |  
`play_entry_tone` | Should the Entry Tone be played | `boolean() | string()` |   | `false` |  
`play_exit_tone` | Should the Exit Tone be played | `boolean() | string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`welcome_prompt.media_id` | Media to play, either Kazoo media ID or URL | `string()` |   | `false` |  
`welcome_prompt.play` | Should the Welcome Prompt be played | `boolean()` | `true` | `false` |  
`welcome_prompt` | Describes how the caller is greeted on entering a conference | `object()` |   | `false` |  

### conferences

Schema for conferences


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bridge_password` | the password used for a conference bridge | `string()` |   | `false` |  
`bridge_username` | the username used for a conference bridge | `string()` |   | `false` |  
`caller_controls` | caller controls (config settings) | `string()` |   | `false` |  
`conference_numbers.[]` |   | `string()` |   | `false` |  
`conference_numbers` | Defines conference numbers that can be used by members or moderators | `array(string())` | `[]` | `false` |  
`controls` | controls | `object()` |   | `false` |  
`domain` | domain | `string()` |   | `false` |  
`focus` | This is a read-only property indicating the media server hosting the conference | `string()` |   | `false` |  
`language` | Prompt language to play in the conference | `string()` |   | `false` |  
`max_members_media` | Media to play when the conference is full | `string()` |   | `false` |  
`max_participants` | The maximum number of participants that can join | `integer()` |   | `false` |  
`member.join_deaf` | Determines if a member will join deaf | `boolean()` | `false` | `false` | `supported`
`member.join_muted` | Determines if a member will join muted | `boolean()` | `true` | `false` | `supported`
`member.numbers.[]` |   | `string()` |   | `false` |  
`member.numbers` | Defines the conference (call in) number(s) for members | `array(string())` | `[]` | `false` |  
`member.pins.[]` |   | `string()` |   | `false` |  
`member.pins` | Defines the pin number(s) for members | `array(string())` | `[]` | `false` |  
`member.play_entry_prompt` | Whether to play the entry prompt on member join | `boolean()` |   | `false` |  
`member` | Defines the discovery (call in) properties for a member | `object()` | `{}` | `false` |  
`moderator.join_deaf` | Determines if a moderator will join deaf | `boolean()` | `false` | `false` |  
`moderator.join_muted` | Determines if a moderator will join muted | `boolean()` | `false` | `false` |  
`moderator.numbers.[]` |   | `string()` |   | `false` |  
`moderator.numbers` | Defines the conference (call in) number(s) for moderators | `array(string())` | `[]` | `false` |  
`moderator.pins.[]` |   | `string()` |   | `false` |  
`moderator.pins` | Defines the pin number(s) for moderators | `array(string())` | `[]` | `false` |  
`moderator` | Defines the discovery (call in) properties for a moderator | `object()` | `{}` | `false` |  
`moderator_controls` | profile on the switch for controlling the conference as a moderator | `string()` |   | `false` |  
`name` | A friendly name for the conference | `string(1..128)` |   | `false` | `supported`
`owner_id` | The user ID who manages this conference | `string(32)` |   | `false` | `supported`
`play_entry_tone` | Whether to play an entry tone, or the entry tone to play | `boolean() | string()` |   | `false` | `supported`
`play_exit_tone` | Whether to play an exit tone, or the exit tone to play | `boolean() | string()` |   | `false` | `supported`
`play_name` | Do we need to announce new conference members? | `boolean()` | `false` | `false` |  
`play_welcome` | Whether to play the welcome prompt | `boolean()` |   | `false` |  
`profile` | Profile configuration | `object()` |   | `false` |  
`profile_name` | conference profile name | `string()` |   | `false` |  
`require_moderator` | does the conference require a moderator | `boolean()` |   | `false` |  
`wait_for_moderator` | should members wait for a moderator before joining the conference | `boolean()` |   | `false` |  

### conferences.profile

Schema for conference profiles


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`alone-sound` | Audio that plays while you are alone in the conference | `string()` |   | `false` |  
`announce-count` | Play member count to conference when above this threshold | `integer()` |   | `false` |  
`caller-controls` | Name of the caller control group | `string()` |   | `false` |  
`comfort-noise` | The volume level of background white noise | `integer()` |   | `false` |  
`energy-level` | Energy level required for audio to be sent to other users | `integer()` |   | `false` |  
`enter-sound` | Audio to play when entering a conference | `string()` |   | `false` |  
`exit-sound` | Audio to play when exiting a conference | `string()` |   | `false` |  
`interval` | Milliseconds per frame | `integer()` |   | `false` |  
`locked-sound` | Audio to play when the conference is locked | `string()` |   | `false` |  
`max-members` | Set the maximum number of members in the conference | `integer()` |   | `false` |  
`max-members-sound` | If max-members has been reached, audio to play to caller instead of joining the conference | `string()` |   | `false` |  
`moderator-controls` | Name of the moderator control group to use | `string()` |   | `false` |  
`moh-sound` | Audio to play, on a loop, while participant count is 1 | `string()` |   | `false` |  
`muted-sound` | Audio to play when muted | `string()` |   | `false` |  
`rate` | Audio sample rate | `integer()` |   | `false` |  
`unmuted-sound` | Audio to play when unmuted | `string()` |   | `false` |  



