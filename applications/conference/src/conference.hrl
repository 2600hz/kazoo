-ifndef(CONFERENCE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_amqp.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(APP_NAME, <<"conference">>).
-define(APP_VERSION, <<"2.0.0">>).

-define(CONFERENCE_CACHE, 'conference_cache').

-define(DEFAULT_PROFILE_CONFIG, [{<<"rate">>, 8000}
                                 ,{<<"caller-controls">>, <<"default">>}
                                 ,{<<"interval">>, 20}
                                 ,{<<"energy-level">>, 20}
                                 ,{<<"comfort-noise">>, 1000}
                                ]).

-define(DEFAULT_CALLER_CONTROLS_CONFIG, [wh_json:from_list([{<<"action">>, <<"vol talk dn">>}
                                                            ,{<<"digits">>, <<"1">>}
                                                           ])
                                         ,wh_json:from_list([{<<"action">>, <<"vol talk zero">>}
                                                             ,{<<"digits">>, <<"2">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"vol talk up">>}
                                                             ,{<<"digits">>, <<"3">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"vol listen dn">>}
                                                             ,{<<"digits">>, <<"4">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"vol listen zero">>}
                                                             ,{<<"digits">>, <<"5">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"vol listen up">>}
                                                             ,{<<"digits">>, <<"6">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"energy dn">>}
                                                             ,{<<"digits">>, <<"7">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"energy equ">>}
                                                             ,{<<"digits">>, <<"8">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"energy up">>}
                                                             ,{<<"digits">>, <<"9">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"mute">>}
                                                             ,{<<"digits">>, <<"0">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"deaf mute">>}
                                                             ,{<<"digits">>, <<"*">>}
                                                            ])
                                         ,wh_json:from_list([{<<"action">>, <<"hangup">>}
                                                             ,{<<"digits">>, <<"#">>}
                                                            ])
                                        ]).
-define(DEFAULT_ADVERTISE_CONFIG, 'undefined').
-define(DEFAULT_CHAT_CONFIG, 'undefined').

-define(CONFERENCE_HRL, 'true').
-endif.
