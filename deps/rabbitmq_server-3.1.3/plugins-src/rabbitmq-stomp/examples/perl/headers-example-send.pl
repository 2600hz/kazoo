#!/usr/bin/perl -w
# send a message to the queue 'foo'
use Net::Stomp;
my $stomp = Net::Stomp->new({hostname=>'localhost', port=>'61613'});
$stomp->connect({login=>'guest', passcode=>'guest'});
$stomp->send({destination=>'/exchange/amq.headers',
              "X-header1" => ($ARGV[0] or "value1"),
	      bytes_message=>1,
	      body=> ("test message ".time())});
$stomp->disconnect;
