# Say

Text-to-speech is an easy way to read text to the caller

# Play sample

The initial Kazoo JSON to

```php
<?php

header('content-type:application/json');

?>

{
    "module": "tts",
    "data": {
        "text": "Pivot is pretty awesome. Have a great day."
    },
    "children": {
        "_": {
            "module": "response",
            "data": {}
        }
    }
}
```

Here the TTS engine will read the text to the caller and then hang up.
