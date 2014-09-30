/*
Section: Pivot
Title: DTMF Collection
Language: en-US
*/

# Overview

It is possible to collect DTMFs via Kazoo JSON using the `cf_collect_dtmf` callflow action. You can also store the collected DTMF in the custom-named keys to differentiate different collections.

# Collecting DTMF

The initial Kazoo JSON to collect DTMF could look something like:

    <?php

    header('content-type:application/json');

    ?>

    {"module":"tts"
     ,"data":{"text":"Please enter up to four digits."}
     ,"children":{
         "_":{
             "module":"collect_dtmf"
             ,"data":{"max_digits":4, "collection_name":"custom_name"}
             ,"children":{
                 "_":{
                     "module":"pivot"
                     ,"data":{"voice_url":"http://pivot.your.company.com/collected.php"}
                     ,"children":{}
                 }
             }
         }
     }
    }

First, Kazoo will use the TTS engine to say the `text` field. Next, it will wait for the user to press up to 4 DTMF (with `#` being a terminating DTMF that is not included in the collection).
