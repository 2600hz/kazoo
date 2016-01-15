

### Conference

#### The Conference document

A conference document contains fields relating to a Moderator, Members or both.

```json
...
"member": {
    "pins": [
        "1234"
    ],
    "join_muted": false,
    "join_deaf": false,
    "numbers": [
        "51"
    ]
},
...
"moderator": {
    "join_deaf": false,
    "join_muted": false,
    "numbers": [
    ],
    "pins": [
    ]
},
...
"play_entry_tone": false,
...
```

Here we can see values set up for a Member, then for a Moderator.

The last field, **play_entry_tone**, is at the root of the document: meaning this field applies to everyone in the conference.

#### Available fields

* **play_entry_tone** and **play_exit_tone**: can be either a boolean or a non-empty string.
    * `true` means play the default tone when someone joins (or leaves) the conference
    * `false` disables the tone from being played
    * A string like a *tone string* or a *URI to a media file* can be inputed.
