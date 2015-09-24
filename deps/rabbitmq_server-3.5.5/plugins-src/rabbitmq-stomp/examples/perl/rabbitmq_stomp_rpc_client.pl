#!/usr/bin/perl -w

use Net::Stomp;
my $stomp = Net::Stomp->new({hostname=>'localhost', port=>'61613'});
$stomp->connect({login=>'guest', passcode=>'guest'});

my $private_q_name = "/queue/c-" . time() . "-" . rand();

$stomp->subscribe({destination => $private_q_name});
$stomp->send({destination => '/queue/rabbitmq_stomp_rpc_service',
              'reply-to' => $private_q_name,
              body => "request from $private_q_name"});
print "Reply: " . $stomp->receive_frame->body;

$stomp->disconnect;
