#!/bin/bash
cd `dirname $0`
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -sname whistle -setcookie ClueCon -eval "amqp_manager:start()." -eval "amqp_broadcast_dispatcher:start()." -eval "amqp_targeted_dispatcher:start()." -eval "request_resorce_responder:start()." -eval "job_dispatcher:start()."
