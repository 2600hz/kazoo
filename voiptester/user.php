<?php

  /**
   demo-ing an "API" for Whistle
  **/

include 'whistler.php';
   
$w = new Whistler('ClueCon Demo');

$callid = $w->waitForPark("4158867988");

echo 'got callid: ', $callid, "\n";

try {
  echo 'Playing /path/to/audio', "\n";
  $w->play($callid, "/path/to/audio");
  echo 'Playing tone_stream://%(300,0,800)', "\n";
  $w->playTone($callid, "tone_stream://%(300,0,800)");
  echo 'Setting terminator #', "\n";
  $w->setTerminator($callid, "#");
  echo 'Recording caller at /path/to/recording.wav', "\n";
  $w->record($callid, "/path/to/recording.wav");
  echo 'Playing /path/to/recording.wav', "\n";
  $w->play($callid, "/path/to/recording.wav");
  echo 'Hanging up', "\n";
  $w->hangup($callid);
} catch(WhistlerException $we) {
  echo $we->getMessage();
}

echo "Finished call\n";