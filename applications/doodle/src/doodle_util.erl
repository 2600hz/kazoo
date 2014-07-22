 %% @author root
%% @doc @todo Add description to doodle_util.


-module(doodle_util).

-include("doodle.hrl").

-define(SIP_ENDPOINT_ID_KEY(Realm, User), {?MODULE, 'sip_endpoint_id', Realm, User}).
-define(SIP_ENDPOINT_KEY(Realm, User), {?MODULE, 'sip_endpoint', Realm, User}).

-export([endpoint_id_from_sipdb/2, get_endpoint_id_from_sipdb/2]).
-export([endpoint_from_sipdb/2, get_endpoint_from_sipdb/2]).
-export([save_sms/2]).
-export([replay_sms/2]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec save_sms(wh_json:object(), whapps_call:call()) -> 'ok'.
save_sms(JObj, Call) ->
    AccountId = whapps_call:account_id(Call),
    OwnerId = whapps_call:owner_id(Call),
    AuthType = whapps_call:authorizing_type(Call),
    AuthId = whapps_call:authorizing_id(Call),
    Body = wh_json:get_value(<<"Body">>, JObj),
    To = whapps_call:to(Call),
    From = whapps_call:from(Call),
    Request = whapps_call:request(Call),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),

    Doc = props:filter_undefined(
            [
             {<<"_id">>, whapps_call:call_id(Call)}
             ,{<<"pvt_type">>, <<"sms">> }
             ,{<<"account_id">>, AccountId }
             ,{<<"owner_id">>, OwnerId }
             ,{<<"authorization_type">>, AuthType }
             ,{<<"authorization_id">>, AuthId }
             ,{<<"to">>, To }
             ,{<<"to_user">>, ToUser }
             ,{<<"to_realm">>, ToRealm }
             ,{<<"from">>, From }
             ,{<<"from_user">>, FromUser }
             ,{<<"from_realm">>, FromRealm }
             ,{<<"request">>, Request }
             ,{<<"request_user">>, RequestUser }
             ,{<<"request_realm">>, RequestRealm }
             ,{<<"body">>, Body }
             ,{<<"message_id">>, MessageId}
             ,{<<"pvt_created">>, wh_util:current_tstamp()}
             ,{<<"pvt_status">>, <<"pending">>}
             ,{<<"call_id">>, whapps_call:call_id(Call)}
             ,{<<"pvt_call">>, whapps_call:to_json(Call)}
             ,{<<"pvt_json">>, JObj}
            ]),
    {'ok', _JObjSaved} = kazoo_modb:save_doc(AccountId, wh_json:from_list(Doc), []).

-spec endpoint_id_from_sipdb(ne_binary(), ne_binary()) ->
                                    {'ok', ne_binary()} |
                                    {'error', _}.
endpoint_id_from_sipdb(Realm, Username) ->
    case wh_cache:peek_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
           get_endpoint_id_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_id_from_sipdb(ne_binary(), ne_binary()) ->
                                        {'ok', ne_binary(), ne_binary()} |
                                        {'error', _}.
get_endpoint_id_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm), wh_util:to_lower_binary(Username)]}],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
            AccountDb = wh_json:get_value(<<"account_db">>, JObj),
            CacheProps = [{'origin', {'db', ?WH_SIP_DB, EndpointId}}],
            wh_cache:store_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username), {AccountDb, EndpointId}, CacheProps),
            {'ok', EndpointId};
        {'ok', []} ->
            lager:debug("sip username ~s not in sip_db", [Username]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s for owner ids: ~p", [Username, _R]),
            E
    end.

-spec endpoint_from_sipdb(ne_binary(), ne_binary()) ->
                                 {'ok', ne_binary()} |
                                 {'error', _}.
endpoint_from_sipdb(Realm, Username) ->
    case wh_cache:peek_local(?DOODLE_CACHE, ?SIP_ENDPOINT_KEY(Realm, Username)) of
        {'ok', _}=Ok -> Ok;
        {'error', 'not_found'} ->
           get_endpoint_from_sipdb(Realm, Username)
    end.

-spec get_endpoint_from_sipdb(ne_binary(), ne_binary()) ->
                                     {'ok', ne_binary(), ne_binary()} |
                                     {'error', _}.
get_endpoint_from_sipdb(Realm, Username) ->
    ViewOptions = [{'key', [wh_util:to_lower_binary(Realm), wh_util:to_lower_binary(Username)]}, 'include_docs'],
    case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) of
        {'ok', [JObj]} ->
            EndpointId = wh_json:get_value(<<"id">>, JObj),
            CacheProps = [{'origin', {'db', ?WH_SIP_DB, EndpointId}}],
            Doc = wh_json:get_value(<<"doc">>, JObj),
            wh_cache:store_local(?DOODLE_CACHE, ?SIP_ENDPOINT_ID_KEY(Realm, Username), Doc, CacheProps),
            {'ok', Doc};
        {'ok', []} ->
            lager:debug("sip username ~s not in sip_db", [Username]),
            {'error', 'not_found'};
        {'error', _R}=E ->
            lager:warning("unable to lookup sip username ~s in sipdb: ~p", [Username, _R]),
            E
    end.

-spec replay_sms(ne_binary(), ne_binary()) -> any().
replay_sms(AccountId, DocId) ->
    lager:debug("trying to replay sms ~s for account ~s",[DocId, AccountId]),
    {'ok', Doc} = kazoo_modb:open_doc(AccountId, DocId),
    Flow = wh_json:get_value(<<"pvt_call">>, Doc),
    replay_sms_flow(AccountId, DocId, Flow).

replay_sms_flow(_AccountId, _DocId, 'undefined') -> 'ok';
replay_sms_flow(AccountId, DocId, JObj) ->
    lager:debug("replaying sms ~s for account ~s",[DocId, AccountId]),
    Call = whapps_call:from_json(JObj),
    whapps_call:set_account_id(AccountId, Call),
    whapps_call:put_callid(Call),
    lager:info("doodle received sms resume for ~s of account ~s, taking control",[DocId, AccountId]),
    doodle_route_win:maybe_restrict_call(JObj, Call).
