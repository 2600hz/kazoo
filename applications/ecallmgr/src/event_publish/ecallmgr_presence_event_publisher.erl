%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Receives PRESENCE_IN event
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_presence_event_publisher).

-export([init/0]).
-export([publish_presence/1]).

-include("ecallmgr.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = kazoo_bindings:bind(<<"event_stream.publish.call_event.PRESENCE_IN">>, ?MODULE, 'publish_presence'),
    'ok'.

-spec publish_presence(map()) -> any().
publish_presence(#{call_id := UUID, payload := _JObj} = Ctx) ->
    kz_log:put_callid(UUID),
    Routines = [fun check_proto/1
               ,fun check_node/1
               ],
    case lists:all(fun(F) -> F(Ctx) end, Routines) of
        'true' -> build_presence_event(UUID, Ctx);
        'false' -> 'ok'
    end;
publish_presence(#{payload := JObj}) ->
    lager:debug_unsafe("PRESENCE NO CALLID ~s", [kz_json:encode(JObj, ['pretty'])]).

-spec check_proto(map()) -> boolean().
check_proto(#{payload := JObj}) ->
    is_proto_handled(kz_json:get_value(<<"proto">>, JObj)).

-spec is_proto_handled(kz_term:api_binary()) -> boolean().
is_proto_handled(<<"any">>) -> 'true';
is_proto_handled(_Proto) ->
    lager:debug("presence proto ~p not handled", [_Proto]),
    'false'.

-spec check_node(map()) -> boolean().
check_node(#{payload := JObj}) ->
    Node = kz_term:to_binary(node()),
    case ?RESTRICTED_PUBLISHING
        andalso kz_call_event:custom_channel_var(JObj, <<"Ecallmgr-Node">>)
    of
        'false' -> 'true';
        'undefined' -> 'true';
        Node -> 'true';
        _Other -> 'false'
    end.

-spec realm(kz_json:object()) -> kz_term:ne_binary().
realm(JObj) ->
    kz_json:get_first_defined([?GET_CCV(<<"Realm">>)
                              ,<<"variable_sip_invite_domain">>
                              ,<<"variable_sip_auth_realm">>
                              ,<<"variable_sip_to_host">>
                              ]
                             ,JObj
                             ,?DEFAULT_REALM
                             ).

-spec get_user_realm(kz_json:object()) -> {kz_term:ne_binary(), kz_term:ne_binary()}.
get_user_realm(JObj) ->
    case binary:split(from(JObj), <<"@">>, ['global']) of
        [Username, Realm | _] -> {Username, Realm};
        [From] -> {From, realm(JObj)}
    end.

-spec from(kz_json:object()) -> kz_term:ne_binary().
from(JObj) ->
    kz_json:get_first_defined([<<"from">>
                              ,<<"variable_presence_id">>
                              ,<<"Channel-Presence-ID">>
                              ]
                             ,JObj
                             ).

-spec to_user(kz_json:object()) -> kz_term:ne_binary().
to_user(JObj) ->
    to_user(direction(JObj), JObj).

to_user(<<"initiator">>, JObj) ->
    kz_json:get_first_defined([<<"Caller-Destination-Number">>
                              ,<<"variable_sip_to_user">>
                              ]
                             ,JObj
                             );
to_user(<<"recipient">>, JObj) ->
    kz_json:get_first_defined([<<"Caller-Caller-ID-Number">>
                              ,<<"variable_sip_from_user">>
                              ]
                             ,JObj
                             ).

-spec expires(kz_term:ne_binary()) -> integer().
expires(<<"early">>) -> 0;
expires(<<"confirmed">>) -> 0;
expires(<<"terminated">>) -> 20.

-spec build_presence_event(kz_term:api_binary(), map()) -> any().
build_presence_event(UUID, #{payload := JObj}) ->
    ToTag = kz_evt_freeswitch:to_tag(JObj),
    FromTag = kz_evt_freeswitch:from_tag(JObj),

    {FromUser, Realm} = get_user_realm(JObj),
    PresenceId = <<FromUser/binary, "@", Realm/binary>>,
    PresenceURI =  <<"sip:", PresenceId/binary>>,

    ToUser =  to_user(JObj),
    To =  <<ToUser/binary, "@", Realm/binary>>,
    ToURI =  <<"sip:", To/binary>>,

    State = presence_status(JObj),
    Expires = expires(State),
    SwitchURI = kz_evt_freeswitch:switch_uri(JObj),

    Payload = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}

                ,{<<"From">>, PresenceURI}
                ,{<<"From-User">>, FromUser}
                ,{<<"From-Realm">>, Realm}
                ,{<<"From-Tag">>, FromTag}

                ,{<<"To">>, ToURI}
                ,{<<"To-User">>, ToUser}
                ,{<<"To-Realm">>, Realm}
                ,{<<"To-Tag">>, ToTag}
                ,{<<"To-URI">>, PresenceURI}

                ,{<<"Direction">>, direction(JObj)}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, UUID}
                ,{<<"Switch-URI">>, SwitchURI}
                ,{<<"Expires">>, Expires}
                ,{<<"Event-Package">>, <<"dialog">>}
                 | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                ]),
    lager:debug("sending presence ~s to ~s/~s in realm ~s", [State, FromUser, ToUser, Realm]),
    _ = maybe_delay(State),
    kapi_presence:publish_dialog(Payload).

maybe_delay(<<"terminated">>) ->
    timer:sleep(?MILLISECONDS_IN_SECOND);
maybe_delay(_) -> 'ok'.

-spec direction(kz_json:object()) -> kz_term:ne_binary().
direction(JObj) ->
    case kz_json:get_value(<<"Presence-Call-Direction">>, JObj) of
        <<"inbound">> -> <<"initiator">>;
        <<"outbound">> -> <<"recipient">>
    end.

-spec status(kz_json:object()) -> kz_term:ne_binary().
status(JObj) ->
    kz_term:to_lower_binary(kz_json:get_binary_value(<<"status">>, JObj)).

-spec answer_state(kz_json:object()) -> kz_term:ne_binary().
answer_state(JObj) ->
    kz_term:to_lower_binary(kz_json:get_value(<<"Answer-State">>, JObj)).

-spec presence_status(kz_json:object()) -> kz_term:ne_binary().
presence_status(JObj) ->
    Status = status(JObj),
    AnswerState = answer_state(JObj),
    Direction = direction(JObj),
    presence_status(Direction, Status, AnswerState).

-spec presence_status(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
presence_status(_, _, <<"answered">>) -> <<"confirmed">>;
presence_status(_, _, <<"hangup">>) -> <<"terminated">>;
presence_status(_, <<"hangup">>, _) -> <<"terminated">>;
presence_status(Direction, <<"cs_", Status/binary>>, AnswerState) -> presence_status(Direction, Status, AnswerState);
presence_status(<<"initiator">>, _, <<"ringing">>) -> <<"confirmed">>;
presence_status(<<"recipient">>, _, <<"ringing">>) -> <<"early">>;
presence_status(_ , _, AnswerState) -> AnswerState.
