%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019 2600Hz
%%% @doc Dialplan API definitions.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Sponsored by Velvetech LLC, Implemented by SIPLABS LLC
%%% @end
%%%-----------------------------------------------------------------------------
-ifndef(KAPI_DIALPLAN_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").

%% For dialplan messages, what does the Invite-Format param accept as values?
-define(INVITE_FORMAT_TUPLE, {<<"Invite-Format">>
                             ,[<<"username">>, <<"e164">>
                              ,<<"npan">>, <<"1npan">>
                              ,<<"route">>, <<"loopback">>
                              ,<<"contact">>
                              ,<<"endpoint">>, <<"forward">>
                              ,<<"strip_plus">>
                              ]
                             }).

%% Move this MACROS to kapi_dialplan.erl module on next PR because they are only being
%% used by kapi_dialplan macros.
%% For dialplan messages, an optional insert-at tuple is common across all requests
-define(INSERT_AT_TUPLE, {<<"Insert-At">>, [<<"head">>, <<"tail">>, <<"flush">>, <<"now">>]}).
-define(IS_TERMINATOR, fun kapi_dialplan:terminators_v/1).
%% left macros end (to be moved to kapi_dialplan.erl module).

-define(DIAL_METHOD_SINGLE, <<"single">>).
-define(DIAL_METHOD_SIMUL, <<"simultaneous">>).

%% Left only because this macros are being used by kapi_conference.erl module.
%% These macros will be deleted on the next PR that modifies kapi_conference module
%% to use kapi_definition module.
%% TTS
-define(TTS_REQ_HEADERS, [<<"Application-Name">>, <<"Call-ID">>, <<"Text">>]).
-define(OPTIONAL_TTS_REQ_HEADERS, [<<"Conference-ID">>
                                  ,<<"Endless-Playback">>
                                  ,<<"Loop-Count">>
                                  ,<<"Engine">>
                                  ,<<"Group-ID">> % group media together (one DTMF cancels all in group)
                                  ,<<"Insert-At">>
                                  ,<<"Language">>
                                  ,<<"Leg">>
                                  ,<<"Terminators">>
                                  ,<<"Voice">>
                                  ]).
-define(TTS_REQ_TYPES, [{<<"Terminators">>, ?IS_TERMINATOR}
                       ,{<<"Endless-Playback">>, fun kz_term:is_boolean/1}
                       ,{<<"Loop-Count">>, fun kz_term:is_pos_integer/1}
                       ]).

%% Tones Request
-define(TONES_REQ_HEADERS, [<<"Call-ID">>, <<"Application-Name">>, <<"Tones">>]).
-define(OPTIONAL_TONES_REQ_HEADERS, [<<"Conference-ID">>
                                    ,<<"Group-ID">>
                                    ,<<"Insert-At">>
                                    ,<<"Terminators">>
                                    ]).
-define(TONES_REQ_VALUES, [{<<"Event-Category">>, <<"call">>}
                          ,{<<"Event-Name">>, <<"command">>}
                          ,{<<"Application-Name">>, <<"tones">>}
                          ,?INSERT_AT_TUPLE
                          ]).
-define(TONES_REQ_TYPES, [{<<"Tones">>, fun is_list/1}
                         ,{<<"Terminators">>, ?IS_TERMINATOR}
                         ]).
%% left macros end (to be deleted).

-define(KAPI_DIALPLAN_HRL, 'true').
-endif.
