<?php
    header("content-type: text/xml");

echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
if($_REQUEST['Digits'] == "TIMEOUT") {
  echo "<Response><Say>Time out, goodbye</Say><Hangup /></Response>";
} else {
  echo "<Response><Say>You entered " .  . "</Say></Response>";
}
?>