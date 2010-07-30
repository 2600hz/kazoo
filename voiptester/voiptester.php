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

    public function requestResorce($type, $minQuantity, $maxQuanity, $timeout = 10, $options = array()) {
        $options += array(
            'min_quantity' => $minQuantity,
            'max_quanity' => $maxQuanity,
            'codec' => NULL,
            'priority' => 'normal',
            'id' => uniqid()
        );

        // Ensure that the exchanges exists, create if not
        $this->channel->exchange_declare('broadcast_requests', 'fanout', false, false, false);
        $this->channel->exchange_declare('targeted_requests', 'direct', false, false, false);

        // Create an anonymous direct queue for this request
        $replyQueue = $this->channel->queue_declare();
        $this->channel->queue_bind($replyQueue[0], 'targeted_requests', $replyQueue[0]);
        
        // Create a consumer for this anonymous queue to collect the replies
        $consumerTag = $this->channel->basic_consume($replyQueue[0], $options['id'], false, false, false, false, array($this, 'request_resource_reply'));

        // Send the requestResorce message
        $msg = new DomDocument('1.0');
        $root = $msg->createElement('request_resorce');
        $root = $msg->appendChild($root);

        $request = $msg->createElement($type);
        $root->appendChild($request);

        foreach ($options as $k => $v) {
            $param = $msg->createAttribute($k);
            $request->appendChild($param);

            $value = $msg->createTextNode($v);
            $param->appendChild($value);
        }

        $msg = new AMQPMessage($msg->saveXML(), array('content_type' => 'text/xml', 'reply_to' => $replyQueue[0]));
        $this->channel->basic_publish($msg, 'broadcast_requests');
        
        // wait for the replies
        $start = time();
        while(count($this->channel->callbacks)) {
            $this->channel->wait();
            $avaliable = 0;
            if (!empty($this->accumulator[$options['id']])) {
                foreach($this->accumulator[$options['id']] as $reply) {
                    $avaliable += $reply['avaliable'];
                }
            }
            if ($avaliable == $maxQuanity) {
                echo "WOOT! Found the resources we requested!\n";
                print_r($this->accumulator);
                break;
            }
            if (!empty($timeout) && (time() - $start) > $timeout) {
                echo "We are not waiting any longer... you got what you got...\n";
                print_r($this->accumulator);
                break;
            }
        }

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

        $reply = new SimpleXMLElement($body);

        foreach($reply->children() as $resource) {
             $result = array(
                'avaliable' => (string)$resource['avaliable'],
                'cost' => (string)$resource['cost'],
                'hostname' => (string)$resource['hostname'],
            );

            foreach ($resource->children() as $child) {
               $result['contact'] = (array)$child;
            }
            $this->accumulator[(string)$resource['id']][] = $result;
        }
    }
}