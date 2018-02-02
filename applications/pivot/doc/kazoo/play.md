# Play

It is pretty easy to play files, either hosted in your account or accessible via a URI.

## Play sample

The initial Kazoo JSON to play a file could look something like:

```php
<?php

header('content-type:application/json');

?>

{
    "module": "play",
    "data": {
        "id": "media_id"
    },
    "children": {
        "_": {
            "module": "play",
            "data":{
                "id": "http://some.file.server/some/file.mp3"
            },
            "children": {}
        }
    }
}
```

Here we see two play actions, one that uses a media file hosted by Kazoo and one that fetches the file from an HTTP server.
