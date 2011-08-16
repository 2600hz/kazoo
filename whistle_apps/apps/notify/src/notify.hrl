-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("callflow/include/cf_amqp.hrl").

-define(DEFAULT_VM_TEMPLATE, <<"New Voicemail Message\n\nCaller ID: {{caller_id_number}}\nCaller Name: {{caller_id_name}}\n\nCalled To: {{to_user}}   (Originally dialed number)\nCalled On: {{date_called|date:\"l, F j, Y \\a\\t H:i\"}}\n\n\nFor help or questions using your phone or voicemail, please contact support at {{support_number}} or email {{support_email}}">>).
-define(DEFAULT_HTML_VM_TEMPLATE, <<"<html><body><h3>New Voicemail Message</h3><table><tr><td>Caller ID</td><td>{{caller_id_name}} ({{caller_id_number}})</td></tr><tr><td>Callee ID</td><td>{{to_user}} (originally dialed number)</td></tr><tr><td>Call received</td><td>{{date_called|date:\"l, F j, Y \\a\\t H:i\"}}</td></tr></table><p>For help or questions using your phone or voicemail, please contact {{support_number}} or email <a href=\"mailto:{{support_email}}\">Support</a></p></body></html>">>).
-define(DEFAULT_SUPPORT_NUMBER, <<"(415) 886-7950">>).
-define(DEFAULT_SUPPORT_EMAIL, <<"support@2600hz.org">>).
