You will need:

 - GNU make
 - Erlang

 - RabbitMQ broker source distribution, compiled, runnable and on your erlang library path
 - erlang-rfc4627 module, compiled and on your erlang library path

To run the server with the additional HTTP support,

  $ make run

The Makefile defaults to searching for the RabbitMQ broker in
../rabbitmq-server, and this can be overridden by setting
RABBIT_SERVER_SOURCE_ROOT, like so:

  $ make RABBIT_SERVER_SOURCE_ROOT=path/to/rabbitmq-server run

(If just the "../" part is wrong, you can try setting a different
variable, RABBIT_SOURCE_ROOT, to the correct value instead. See the
Makefile for the details of how RABBIT_SERVER_SOURCE_ROOT is
constructed from RABBIT_SOURCE_ROOT.)

The broker should start, and output a series of "starting ..."
messages. A successful start looks like:

  starting database             ...done
  starting core processes       ...done
  starting recovery             ...done
  starting persister            ...done
  starting builtin applications ...done
  starting TCP listeners        ...done
  starting HTTP-JSON-listeners  ...done

  broker running

If erlang-rfc4627 is not compiled, or not on the erlang search path,
you will see:

  starting database             ...done
  starting core processes       ...done
  starting recovery             ...done
  starting persister            ...done
  starting builtin applications ...done
  starting TCP listeners        ...done
  starting HTTP-JSON-listeners  ...{"init terminating in do_boot",{{nocatch,{error,{cannot_start_application,rabbit,{bad_return,{{rabbit,start,[normal,[]]},{'EXIT',{undef,[{mod_jsonrpc,start,[]},{rabbit_http,kickstart,0},{rabbit,'-start/2-fun-0-',1},{lists,foreach,2},{rabbit,start,2},{application_master,start_it_old,4}]}}}}}}},[{init,start_it,1},{init,start_em,1}]}}

  Crash dump was written to: erl_crash.dump
  init terminating in do_boot ()

And finally, if the RABBIT_SERVER_SOURCE_ROOT is wrong, or it is right
but the code is not compiled or not on your erlang search path, you
will see:

  erlc -I ../rabbitmq-server/include -I include -o ebin -Wall +debug_info  src/rabbit_http_channel.erl
  src/rabbit_http_channel.erl:28: can't find include file "rabbit.hrl"
  src/rabbit_http_channel.erl:29: can't find include file "rabbit_framing.hrl"
  src/rabbit_http_channel.erl:133: record content undefined
  src/rabbit_http_channel.erl:136: variable 'Props' is unbound
  src/rabbit_http_channel.erl:139: variable 'BodyFragmentsRev' is unbound
  src/rabbit_http_channel.erl:146: record 'P_basic' undefined
  src/rabbit_http_channel.erl:198: record content undefined
  src/rabbit_http_channel.erl:220: record 'P_basic' undefined
  src/rabbit_http_channel.erl:240: record 'channel.open' undefined
  src/rabbit_http_channel.erl:263: record 'P_basic' undefined
  src/rabbit_http_channel.erl:276: record 'channel.open_ok' undefined
  src/rabbit_http_channel.erl:146: Warning: function build_props/1 is unused
  src/rabbit_http_channel.erl:197: Warning: variable 'Bin' is unused
  src/rabbit_http_channel.erl:197: Warning: variable 'Props' is unused
  make: *** [ebin/rabbit_http_channel.beam] Error 1

All that aside, once the server starts successfully, including a
"...done" for the "starting HTTP-JSON-listeners" line, you should be
able to visit

  http://localhost:15670/

and get a webpage. Clicking on "Simple JSONRPC test" will run a small
test application. Successful output of http://localhost:15670/test/index.html
at the time of writing is:

  test_main
  {"installGenericProxy":{"name":"open","idempotent":false,"params":[{"name":"username","type":"str"},{"name":"password","type":"str"},{"name":"sessionTimeout","type":"num"},{"name":"virtualHost","type":"str"}]}}
  open
  {"installGenericProxy":{"name":"poll","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"close","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"call","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"}]}}
  {"installGenericProxy":{"name":"cast","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"},{"name":"content","type":"str"},{"name":"props","type":"arr"}]}}
  handle_channel_ready
  {"q1":"test-queue-1a"}
  {"q2":"test-queue-1b"}
  {"consumeOk":"aa-cons-tag1"}
  {"delivery":{"content":"hello, world","delivery_tag":1,"redelivered":false,"exchange":"","routing_key":"test-queue-1a","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":null,"expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"cancelOk":"aa-cons-tag1"}
  {"delivery2":{"content":"hello, world, again! pub 2","delivery_tag":2,"redelivered":false,"exchange":"","routing_key":"test-queue-1b","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":"something22","expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"installGenericProxy":{"name":"poll","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"close","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"call","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"}]}}
  {"installGenericProxy":{"name":"cast","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"},{"name":"content","type":"str"},{"name":"props","type":"arr"}]}}
  test basic.cancel compliance
  queue declare OK
  {"delivery4":{"content":"One","delivery_tag":1,"redelivered":false,"exchange":"","routing_key":"test-queue-4","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":null,"expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"never existed":"this-never-existed"}
  {"cancelled":"my-consumer"}

The source to the test program is in

  server_root/htdocs/test/test.js and
  server_root/htdocs/test/index.html

Tony Garnock-Jones <tonyg@rabbitmq.com>
1 May 2008
http://lists.rabbitmq.com/cgi-bin/mailman/listinfo/

