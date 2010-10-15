<?php
  /*
$w = new Whistler('ClueCon Demo');
$callid = $w->waitForPark("XXX");

try {
  $w->play($callid, "/path/to/audio");
  $w->playTone($callid, "tone_stream://%(300,0,800)");
  $w->setTerminator($callid, "#");
  $w->record($callid, "/path/to/recording.wav");
  $w->play($callid, "/path/to/recording.wav");
  $w->hangup($callid);
} catch(WhistleException $we) {
  echo $we->getMessage();
}
*/

  /**
   * Very basic API class for interfacing with Whistle.
   * Really only implementing enough for the demo for now.
   *
   * Public API
   **/

include('amqp.inc');

class Whistler {
  protected $app_name = 'Whistle Client';

  private $amqp_options = array(
				'host' => 'fs1.voicebus.net'
				,'port' => 5672
				,'vhost' => '/'
				,'user' => 'guest'
				,'pass' => 'guest'
				);

  protected $connection = NULL; // connection to AMQP broker

  protected $channel = NULL; // channel to communicate with AMQP broker

  protected $w_uuid = 0; // our unique id used for our targetedXc queue

  protected $inbound_number = ''; // what number are we waiting for

  protected $incoming_exchange = 'targeted'; // what exchange are we subscribing to?

  protected $incoming_queue = ''; // what queue do we consume from?

  protected $message_queue = array(); // put messages from our queue in here
  
  public function __construct($app_name='Whistle Client') {
    $this->w_uuid = mt_rand();

    $this->app_name = $app_name;

    $conn = new AMQPConnection($this->amqp_options['host'], $this->amqp_options['port'], $this->amqp_options['user'], $this->amqp_options['pass']);
    $this->channel = $conn->channel();
    $this->channel->access_request($this->amqp_options['vhost'], false, false, true, true);

    $this->channel->exchange_declare('broadcast', 'fanout', false, false, false);
    $this->channel->exchange_declare($this->incoming_exchange, 'direct', false, false, false);

    $this->incoming_queue = $this->channel->queue_declare($this->incoming_exchange . '.' . $this->w_uuid);
    $this->channel->queue_bind($this->incoming_queue[0], $this->incoming_exchange, $this->incoming_queue[0]);

    // Create a consumer for this anonymous queue to collect the replies
    $consumerTag = $this->channel->basic_consume($this->incoming_queue[0], mt_rand()
						 ,false, false, false, false
						 ,array($this, 'message_ready'));
  }

  public function  __destruct() {
    if ($this->channel) {
      $this->channel->close();
    }

    if ($this->connection) {
      $this->connection->close();
    }
  }

  public function waitForPark($number) {
    $this->inbound_number = $number;
    $request_id = $this->registerNumberForPark($number);
    $this->wait();

    $msg = $this->getNextMessage($request_id);

    while ( is_array($msg) && empty($msg['callid']) ) {
      $msg = $this->getNextMessage();
    }

    if ( ! is_array($msg) ) {
      echo 'Failed to find msg: '; print_r($msg);
      throw new WhistleException('Failed to park for ' . $number);
    }

    return $msg['callid'];
  }

  protected function registerNumberForPark($number) {
    $payload = array(
		     'request_id' => mt_rand()
		     ,'resp_exchange_id' => $this->incoming_exchange
		     ,'resp_queue_id' => $this->incoming_queue[0]
		     ,'app' => $this->app_name
		     ,'action' => 'request'
		     ,'category' => 'event'
		     ,'type' => 'CHANNEL_PARK'
		     ,'command' => 'register'
		     ,'incoming_number' => $number
		     );
    $msg = new AMQPMessage(json_encode($payload), array('content_type' => 'application/json'
						  ,'reply_to' => $this->incoming_queue[0]));
    $this->channel->basic_publish($msg, 'broadcast');
    return $payload['request_id'];
  }

  private function getNextMessage($request_id=NULL) {
    if ( count($this->message_queue) == 0 ) return NULL;

    if ( $request_id == NULL ) {
      return array_shift($this->message_queue);
    }

    foreach ( $this->message_queue as $msg ) {
      if ( ! empty($msg['request_id']) && $msg['request_id'] == $msg ) return $msg;
    }

    return NULL;
  }

  private function wait() {
    $this->channel->wait();
    while ( ! $this->isMsgReady() ) ;
  }

  private function message_ready($msg) {
    $deliveryTag = $msg->delivery_info['delivery_tag'];
    $body = $msg->body;

    $this->channel->basic_ack($deliveryTag);

    array_push($this->message_queue, $body);
  }
}