<?php
    header("content-type: text/xml");
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
?>
<Response>
    <Gather action="/gather.2.process.php" method="GET">
        <Say>
            Please enter your account number, 
            followed by the pound sign
        </Say>
    </Gather>
    <Redirect method="GET">
        /gather.2.process.php?Digits=TIMEOUT
    </Redirect>
</Response>