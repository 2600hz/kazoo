## Sound Touch

### About Sound Touch

#### Schema

Pitch-shift and other audio effects



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Whether to start or stop touching the sound | `string('start' | 'stop')` |   | `false`
`adjust_in_octaves` | Adjust the pitch in octaves | `number()` | `0` | `false`
`adjust_in_semitones` | Adjust the pitch in semitones | `number()` | `0` | `false`
`hook_dtmf` | Enable DTMF control of audio modifications | `boolean()` | `false` | `false`
`pitch` | Set the pitch directly (lower number = lower tone) | `number()` | `1.0` | `false`
`rate` | Set the rate directly | `number()` | `1.0` | `false`
`sending_leg` | Apply the filter to the sending leg | `boolean()` | `false` | `false`
`tempo` | Set the tempo directly | `number()` | `1.0` | `false`



