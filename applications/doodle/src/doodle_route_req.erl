%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(doodle_route_req).

-export([handle_req/2]).

-include("doodle.hrl").

-define(RESOURCE_TYPES_HANDLED,[<<"SMS">>]).


-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),
    Call = whapps_call:from_route_req(JObj),
    case is_binary(whapps_call:account_id(Call))
             andalso resource_allowed(Call)
        of
        'true' ->
            lager:info("received a request asking if doodle can route this message"),
            AllowNoMatch = allow_no_match(Call),
            case cf_util:lookup_callflow(Call) of
                %% if NoMatch is false then allow the callflow or if it is true and we are able allowed
                %% to use it for this call
                {'ok', Flow, NoMatch} when (not NoMatch) orelse AllowNoMatch ->
                    maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch);
                {'ok', _, 'true'} ->
                    lager:info("only available callflow is a nomatch for a unauthorized call", []);
                {'error', R} ->
                    lager:info("unable to find callflow ~p", [R])
            end;
        
        'false' ->
            'ok'
    end.

-spec resource_allowed(whapps_call:call()) -> boolean().
resource_allowed(Call) ->
    is_resource_allowed(whapps_call:resource_type(Call)).

-spec is_resource_allowed(api_binary()) -> boolean().
is_resource_allowed('undefined') -> 'true';
is_resource_allowed(ResourceType) ->
    lists:member(ResourceType, ?RESOURCE_TYPES_HANDLED).

-spec allow_no_match(whapps_call:call()) -> boolean().
allow_no_match(Call) ->
    whapps_call:custom_channel_var(<<"Referred-By">>, Call) =/= 'undefined'
        orelse allow_no_match_type(Call).

-spec allow_no_match_type(whapps_call:call()) -> boolean().
allow_no_match_type(Call) ->
    case whapps_call:authorizing_type(Call) of
        'undefined' -> 'false';
        <<"resource">> -> 'false';
        <<"sys_info">> -> 'false';
        _ -> 'true'
    end.

   
maybe_reply_to_req(JObj, Props, Call, Flow, NoMatch) ->
    lager:info("callflow ~s in ~s satisfies request", [wh_json:get_value(<<"_id">>, Flow)
                                                       ,whapps_call:account_id(Call)
                                                      ]),
    ControllerQ = props:get_value('queue', Props),
    AccountId = whapps_call:account_id(Call),
    save_sms(AccountId, JObj, Props),
    send_route_response(Flow, JObj, ControllerQ, Call).


-spec send_route_response(wh_json:object(), wh_json:object(), ne_binary(), whapps_call:call()) -> 'ok'.
send_route_response(Flow, JObj, Q, Call) ->
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"sms">>}
                                   | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    whapps_util:amqp_pool_send(Resp, Publisher),
    lager:info("doodle knows how to route the message! sent sms response").


build_sms_flow(JObj, Props, Call, NoMatch, NoMatch) ->
    lager:info("building off-net request").
    


-spec save_sms(ne_binary(), wh_json:object(), wh_proplist()) -> 'ok'.
save_sms(AccountId, JObj, Props) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    lager:info("DOODLE ~p",[CCVs]),
    OwnerId = wh_json:get_value(<<"Owner-ID">>, CCVs),
    AuthType = wh_json:get_value(<<"Authorizing-Type">>, CCVs),
    AuthId = wh_json:get_value(<<"Authorizing-ID">>, CCVs),
    Body = wh_json:get_value(<<"Body">>, JObj),
    To = wh_json:get_value(<<"To">>, JObj),
    From = wh_json:get_value(<<"From">>, JObj),
    Request = wh_json:get_value(<<"Request">>, JObj),
    [ToUser, ToRealm] = binary:split(To, <<"@">>),
    [FromUser, FromRealm] = binary:split(From, <<"@">>),
    [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),
    MessageId = wh_json:get_value(<<"Message-ID">>, JObj),
    
    Doc = props:filter_undefined([
           {<<"pvt_type">>, <<"sms">>}
          ,{<<"account_id">>, AccountId}
          ,{<<"owner_id">>, OwnerId}
          ,{AuthType, AuthId}
          ,{<<"to">>, To}
          ,{<<"from">>, From}
          ,{<<"request">>, Request}
          ,{<<"body">>, Body}
          ,{<<"direction">>, <<"outbound">>}
          ,{<<"inception">>, <<"on-net">>}
          ,{<<"Message_id">>, MessageId}
          ,{<<"pvt_created">>, wh_util:current_tstamp()}
          ,{<<"status">>, <<"queued">>}
           ]),
        
    
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    lager:info("saving ~p into ~s",[Doc, AccountDb]),
    {'ok', JObjSaved} = couch_mgr:save_doc(AccountDb, wh_json:from_list(Doc)),
    save_sms_worker(AccountDb, wh_json:get_value(<<"_id">>, JObjSaved), Doc).

-spec save_sms_worker(ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
save_sms_worker(AccountDb, DocId, Doc) ->
    Doc1 = [ 
            {<<"account_db">>, AccountDb}
           ,{<<"sms_id">>, DocId}
           ,{<<"retries">>, 20}
           ,{<<"pvt_job_status">>, <<"pending">>}
           | Doc],
    couch_mgr:save_doc(?DOODLE_DB, wh_json:from_list(Doc1)).
  