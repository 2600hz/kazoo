<?php
    header("content-type: text/xml");
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
?>
<Response>
  <Dial>
    <Conference startConferenceOnEnter="true" endConferenceOnExit="true">
      moderated-conference-room
    </Conference>
  </Dial>
</Response>