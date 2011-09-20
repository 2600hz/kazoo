-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("rabbitmq_erlang_client/include/amqp_client.hrl").

-define(EXCHANGE_CONFERENCE, <<"conference">>).
-define(TYPE_CONFERENCE, <<"topic">>).

-define(APP_NAME, <<"conference">>).
-define(APP_VERSION, <<"0.0.6">>).

-record(prompts, {greeting = <<"/system_media/conf-welcome">>
                  ,request_id = <<"/system_media/conf-enter_conf_number">>
                  ,request_pin = <<"/system_media/conf-enter_conf_pin">>
                  ,incorrect_id = <<"/system_media/conf-bad_conf">>
                  ,incorrect_pin = <<"/system_media/conf-bad_pin">>
                  ,to_many_attempts = <<"/system_media/conf-enter_conf_number">>
                  ,alone_enter = <<"/system_media/conf-alone">>
                  ,single_enter = <<"/system_media/conf-single">>
                  ,multiple_enter_1 = <<"/system_media/conf-there_are">>
                  ,multiple_enter_2 = <<"/system_media/conf-other_participants">>
                  ,announce_join = <<"tone_stream://%(200,0,500,600,700)">>
                  ,announce_leave = <<"tone_stream://%(500,0,300,200,100,50,25)">>
                  ,muted = <<"/system_media/conf-muted">>
                  ,unmuted = <<"/system_media/conf-unmuted">>
                  ,deaf = <<"/system_media/conf-deaf">>
                  ,undeaf = <<"/system_media/conf-undeaf">>
                 }).

-record(control, {mute = <<"1">>
                  ,unmute = <<"2">>
                  ,deaf = <<"3">>
                  ,undeaf = <<"4">>
                  ,toggle_mute = <<"0">>
                  ,toggle_deaf = <<"*">>
                  ,hangup = <<"#">>
                 }).

-record(participant, {call_id = undefined
                      ,control_q = undefined
                      ,bridge_id = undefined
                      ,bridge_ctrl = undefined
                      ,moderator = false
                      ,muted = false
                      ,deaf = false
                      ,participant_id = 0
                     }).

-record(conf, {service = undefined
               ,amqp_q = undefined
               ,conf_id = undefined
               ,route = undefined
               ,focus = undefined
               ,ctrl_q = []
               ,auth_pwd = <<"\/\/|-|157L3_(0|\|Ph3R3|\|(3">>
               ,member_pins = []
               ,moderator_pins = []
               ,member_join_muted = true
               ,member_join_deaf = false
               ,moderator_join_muted = false
               ,moderator_join_deaf = false
               ,max_members = 0
               ,require_moderator = false
               ,wait_for_moderator = false
               ,participants = dict:new()
               ,prompts = #prompts{}
               ,controls = #control{}
              }).
