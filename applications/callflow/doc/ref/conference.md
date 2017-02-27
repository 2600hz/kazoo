## Conference

### About Conference

### Schema

Validator for the Conference callflow element

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`config` | Build an ad-hoc conference using the conferences JSON schema | `object` |   | `false`
`id` | Kazoo ID of the conference | `string(32)` |   | `false`
`moderator` | Is the caller entering the conference as a moderator | `boolean` | `false` | `false`
`play_entry_tone` | Should the Entry Tone be played | `boolean, string` | `true` | `false`
`play_exit_tone` | Should the Exit Tone be played | `boolean, string` | `true` | `false`
`welcome_prompt` | Describes how the caller is greeted on entering a conference | `object` |   | `false`
`welcome_prompt.media_id` | Media to play, either Kazoo media ID or URL | `string` |   | `false`
`welcome_prompt.play` | Should the Welcome Prompt be played | `boolean` | `true` | `false`
