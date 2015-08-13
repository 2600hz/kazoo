<?php

header('content-type: application/json');

$caller_id = $_REQUEST['Caller-ID-Number'];

switch(substr($caller_id, 0, 3)) {
    case "415": $conf_id="Safe"; break;
    default: $conf_id="Axed";
}

?>
    {"module":"tts"
     ,"data":{
         "text":"<?= $conf_id ?>"
     }
     ,"children":{
         "_":{
             "module":"conference"
             ,"data":{
                 "welcome_prompt":{
                     "play":true
                     ,"media_id":"<?= $conf_id == "Safe" ? "fb7b84c087d6ea386506b68eba8db3d1" : "e75313323cc69129bce08c1fcc4f4ec8" ?>"
                 }
                 ,"config":{
                     "name":"Conf<?= $conf_id ?>"
                 }
             }
             ,"children":{}
         }
     }

    }
