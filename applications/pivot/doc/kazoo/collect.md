# Collect

It is possible to collect DTMFs via Kazoo JSON using the `cf_collect_dtmf` callflow action. You can also store the collected DTMF in the custom-named keys to differentiate different collections.

# Collecting DTMF

The initial Kazoo JSON to collect DTMF could look something like:

```php
<?php

header('content-type:application/json');

?>
{
    "module":"tts",
    "data":{
        "text": "Please enter up to four digits."
    },
    "children": {
        "_": {
            "module": "collect_dtmf",
            "data": {
                "max_digits": 4,
                "collection_name": "custom_name"
            },
            "children": {
                "_": {
                    "module": "pivot",
                    "data": {
                        "voice_url": "http://pivot.your.company.com/collected.php"
                    },
                    "children": {}
                }
            }
        }
    }
}
```

First, Kazoo will use the TTS engine to say the `text` field. Next, it will wait for the user to press up to 4 DTMF (with `#` being a terminating DTMF that is not included in the collection). Finally, a second pivot request will be made the the `collected.php` script on your server.

This is a basic menu! Congrats, you can build custom IVRs!

## Processing collected DTMF

Here is demonstrated speaking back the digits pressed to the caller; you could obviously key off the DTMF to do whatever further call processing.

```php
<?php

header('content-type:application/json');

$dtmf = $_REQUEST['Digits'];

if ( empty($dtmf) ) {
?>

{
    "module": "tts",
    "data": {
        "text": "We didn't get that"
    },
    "children": {}
}

<?php } else if ( is_string($dtmf) ) { ?>

{
    "module": "tts",
    "data": {
        "text": "You typed <?= $dtmf ?>"
    },
    "children": {}
}

<?php } else { ?>

{
    "module": "tts",
    "data": {
        "text": "You typed <?= $dtmf['custom_name'] ?>"
    },
    "children": {}
}

<?php } ?>
```

The `is_string($dtmf)` check is to support the old way of returning DTMF in the Pivot request. Otherwise, you should receive an array of DTMF collections, indexed by the key name supplied ("default" if you didn't specify one).
