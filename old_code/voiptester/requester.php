<?php
    require_once('voiptester.php');

//rr(fs_socket_manager). fs_socket_manager:start(#host{hostname = "localhost", port = 8021, auth = "ClueCon"}).
    $toolkit = new voiptester('fs1.voicebus.net');
    $toolkit->requestResorce('voice_channels', 1, 1);

/*    $uuids = $toolkit->spinUpCall(5, array('123123', '234234', '34554', '456456', '6575757'));
    while ($toolkit->receiveEvents($uuids)) do {
	// blah
	};
    $device = $toolkit->deviceInfo('kanderson', 'pbx.switchfreedom.com');*/


?>
