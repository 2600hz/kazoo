<?php

require_once('amqp.inc');

class voiptester {
  protected $connection = NULL;

  protected $channel = NULL;

  protected $accumulator = NULL;

  public function  __construct($host, $user = 'guest', $pass = 'guest', $options = array()) {
    $options = array(
		     'port' => 5672,
		     'vhost' => '/'
		     );

    $conn = new AMQPConnection($host, $options['port'], $user, $pass);
    $this->channel = $conn->channel();
    $this->channel->access_request($options['vhost'], false, false, true, true);
  }

  public function  __destruct() {
    if ($this->channel) {
      $this->channel->close();
    }

    if ($this->connection) {
      $this->connection->close();
    }
  }

  public function requestResource($type) {
    $payload = array('request-id' => uniqid()
		     ,'resp-exchange-id' => 'targeted'
		     ,'resp-queue-id' => 'rscmgr_req_testerQ'
		     ,'app' => 'rscmgr_req_tester'
		     ,'action' => 'request'
		     ,'category' => 'resource'
		     ,'type'  => $type // 'channel' | message | media,
		     ,'command' => 'find'
		     );

    // Ensure that the exchanges exists, create if not
    // send request to
    $this->channel->exchange_declare('broadcast', 'fanout', false, false, false);
    // receive resp from
    $this->channel->exchange_declare('targeted', 'direct', false, false, false);

    // Create an anonymous direct queue for this request
    $replyQueue = $this->channel->queue_declare('rscmgr_req_testerQ');
    $this->channel->queue_bind($replyQueue[0], 'targeted', $replyQueue[0]);

    // Create a consumer for this anonymous queue to collect the replies
    $consumerTag = $this->channel->basic_consume($replyQueue[0], $payload['request-id']
						 ,false, false, false, false
						 ,array($this, 'request_resource_reply'));

    $msg = new AMQPMessage(json_encode($payload), array('content_type' => 'application/json'
							,'reply_to' => $replyQueue[0]));
    print_r($msg);
    echo 'Hey, basic_publish: ', $this->channel->basic_publish($msg, 'broadcast'), "\n";

    // wait for the replies
    $start = time();
    $timeout = 1000;

    while(count($this->channel->callbacks)) {
      echo 'call wait', "\n";
      $this->channel->wait();
      if (!empty($timeout) && (time() - $start) > $timeout) {
	echo "We are not waiting any longer... you got what you got...\n";
	break;
      }
    }

    return;

    $this->channel->basic_cancel($consumerTag);

    if (!empty($this->accumulator[$options['id']])) {
      foreach($this->accumulator[$options['id']] as $reply) {
	if (empty($reply['hostname'])) continue;
        
	// Send the or message
	$msg = new DomDocument('1.0');
	$root = $msg->createElement('preform_action');
	$root = $msg->appendChild($root);

	$fsApi = $msg->createElement('job');
	$root->appendChild($fsApi);

	$param = $msg->createAttribute('id');
	$fsApi->appendChild($param);

	$value = $msg->createTextNode($options['id']);
	$param->appendChild($value);

	$param = $msg->createAttribute('priority');
	$fsApi->appendChild($param);

	$value = $msg->createTextNode('normal');
	$param->appendChild($value);

	$param = $msg->createAttribute('reply_to');
	$fsApi->appendChild($param);

	$value = $msg->createTextNode($replyQueue[0]);
	$param->appendChild($value);

	for ($i = 0; $i < $reply['avaliable']; $i ++) {
	  $action = $msg->createElement('fs_api');
	  $fsApi->appendChild($action);

	  $param = $msg->createAttribute('application');
	  $action->appendChild($param);

	  $value = $msg->createTextNode('status');
	  $param->appendChild($value);

	  $param = $msg->createAttribute('data');
	  $action->appendChild($param);

	  $value = $msg->createTextNode('');
	  $param->appendChild($value);
	}

	$msg = new AMQPMessage($msg->saveXML(), array('content_type' => 'text/xml'));
	$this->channel->basic_publish($msg, 'targeted_requests', 'targeted.' .$reply['hostname']);
      }
    }
  }

  function request_resource_reply($msg) {
    $deliveryTag = $msg->delivery_info['delivery_tag'];
    $body = $msg->body;

    $this->channel->basic_ack($deliveryTag);

    echo 'rrr ', print_r($msg);
  }
}

$toolkit = new voiptester('fs1.voicebus.net');
$toolkit->requestResource('channels', 1, 1);