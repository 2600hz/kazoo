-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"conference">>).
-define(APP_VERSION, <<"0.0.6">>).

-record(prompts, {greeting = <<"/system_media/conf-welcome">>
                  ,request_id = <<"/system_media/conf-enter_conf_number">>
                  ,request_pin = <<"/system_media/conf-enter_conf_pin">>
                  ,incorrect_id = <<"/system_media/conf-bad_conf">>
                  ,incorrect_pin = <<"/system_media/conf-bad_pin">>
                  ,to_many_attempts = <<"/system_media/conf-enter_conf_number">>
                  ,generic_joining = <<"/system_media/conf-joining_conference">>
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

-record(conference22, {id = undefined
                     ,focus = undefined
                     ,controller_q = undefined
                     ,bridge_password = <<"\/\/|-|157L3_(0|\|Ph3R3|\|(3">>
                     ,bridge_username = <<"test">>
                     ,member_pins = []
                     ,moderator_pins = []
                     ,join_as_moderator = undefined
                     ,member_join_muted = true
                     ,member_join_deaf = false
                     ,moderator_join_muted = false
                     ,moderator_join_deaf = false
                     ,max_members = 0
                     ,require_moderator = false
                     ,wait_for_moderator = false
                     ,member_play_name = false
                     ,conference_doc = wh_json:new()
                    }).
