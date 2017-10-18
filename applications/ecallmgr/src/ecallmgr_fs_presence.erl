%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz INC
%%% @doc
%%% Receives PRESENCE_IN event
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_presence).

-export([init/0]).
-export([publish_presence/1]).

-include("ecallmgr.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.publish.call_event.PRESENCE_IN">>, ?MODULE, 'publish_presence'),
    'ok'.

-spec publish_presence(map()) -> any().
publish_presence(#{call_id := UUID} = Ctx) ->
    kz_util:put_callid(UUID),
    Routines = [fun check_proto/1
               ],
    case lists:all(fun(F) -> F(Ctx) end, Routines) of
        'true' -> build_presence_event(UUID, Ctx);
        'false' -> 'ok'
    end.

-spec check_proto(map()) -> boolean().
check_proto(#{payload := JObj}) ->
    is_proto_handled(kz_json:get_value(<<"proto">>, JObj)).

-spec is_proto_handled(api_binary()) -> boolean().
is_proto_handled(<<"any">>) -> 'true';
is_proto_handled(_Proto) ->
    lager:debug("presence proto ~p not handled", [_Proto]),
    'false'.

-spec realm(kz_json:object()) -> ne_binary().
realm(JObj) ->
    kz_json:get_first_defined([?GET_CCV(<<"Realm">>)
                              ,<<"variable_sip_invite_domain">>
                              ,<<"variable_sip_auth_realm">>
                              ,<<"variable_sip_to_host">>
                              ], JObj, ?DEFAULT_REALM).

-spec get_user_realm(kz_json:object()) -> {ne_binary(), ne_binary()}.
get_user_realm(JObj) ->
    case binary:split(from(JObj), <<"@">>, ['global']) of
        [Username, Realm | _] -> {Username, Realm};
        [From] -> {From, realm(JObj)}
    end.

-spec from(kz_json:object()) -> ne_binary().
from(JObj) ->
    kz_json:get_first_defined([<<"from">>
                              ,<<"variable_presence_id">>
                              ,<<"Channel-Presence-ID">>
                              ], JObj).

-spec to_user(kz_json:object()) -> ne_binary().
to_user(JObj) ->
    to_user(direction(JObj), JObj).

to_user(<<"initiator">>, JObj) ->
    kz_json:get_first_defined([<<"Caller-Destination-Number">>
                              ,<<"variable_sip_to_user">>
                              ], JObj);
to_user(<<"recipient">>, JObj) ->
    kz_json:get_first_defined([<<"Caller-Caller-ID-Number">>
                              ,<<"variable_sip_from_user">>
                              ], JObj).

-spec expires(ne_binary()) -> integer().
expires(<<"early">>) -> 0;
expires(<<"confirmed">>) -> 0;
expires(<<"terminated">>) -> 20.

-spec build_presence_event(api_binary(), map()) -> any().
build_presence_event(UUID, #{payload := JObj}) ->
    ToTag = kzd_freeswitch:to_tag(JObj),
    FromTag = kzd_freeswitch:from_tag(JObj),

    {FromUser, Realm} = get_user_realm(JObj),
    PresenceId = <<FromUser/binary, "@", Realm/binary>>,
    PresenceURI =  <<"sip:", PresenceId/binary>>,

    ToUser =  to_user(JObj),
    To =  <<ToUser/binary, "@", Realm/binary>>,
    ToURI =  <<"sip:", To/binary>>,

    State = presence_status(JObj),
    Expires = expires(State),
    SwitchURI = kzd_freeswitch:switch_uri(JObj),

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
    kz_amqp_worker:cast(Payload, fun kapi_presence:publish_dialog/1).

-spec direction(kz_json:object()) -> ne_binary().
direction(JObj) ->
    case kz_json:get_value(<<"Presence-Call-Direction">>, JObj) of
        <<"inbound">> -> <<"initiator">>;
        <<"outbound">> -> <<"recipient">>
    end.

-spec status(kz_json:object()) -> ne_binary().
status(JObj) ->
    kz_term:to_lower_binary(kz_json:get_binary_value(<<"status">>, JObj)).

-spec answer_state(kz_json:object()) -> ne_binary().
answer_state(JObj) ->
    kz_term:to_lower_binary(kz_json:get_value(<<"Answer-State">>, JObj)).

-spec presence_status(kz_json:object()) -> ne_binary().
presence_status(JObj) ->
    Status = status(JObj),
    AnswerState = answer_state(JObj),
    Direction = direction(JObj),
    presence_status(Direction, Status, AnswerState).

-spec presence_status(ne_binary(), ne_binary(), ne_binary()) -> ne_binary().
presence_status(_, _, <<"answered">>) -> <<"confirmed">>;
presence_status(_, _, <<"hangup">>) -> <<"terminated">>;
presence_status(_, <<"hangup">>, _) -> <<"terminated">>;
presence_status(Direction, <<"cs_", Status/binary>>, AnswerState) -> presence_status(Direction, Status, AnswerState);
presence_status(<<"initiator">>, _, <<"ringing">>) -> <<"confirmed">>;
presence_status(<<"recipient">>, _, <<"ringing">>) -> <<"early">>;
presence_status(_ , _, AnswerState) -> AnswerState.
