%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle route requests
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_inbound).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    _ = whapps_util:put_callid(JObj),
    case wh_json:get_ne_value(?CCV(<<"Account-ID">>), JObj) of
        'undefined' -> maybe_relay_request(JObj);
        _AcctID -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle a request inbound from offnet
%% @end
%%--------------------------------------------------------------------
maybe_relay_request(JObj) ->
    Number = stepswitch_util:get_inbound_destination(JObj),
    case stepswitch_util:lookup_number(Number) of
        {'error', _R} -> 
            lager:info("unable to determine account for ~s: ~p", [Number, _R]);
        {'ok', _, Props} ->
            lager:debug("relaying route request"),
            Routines = [fun set_account_id/2
                        ,fun set_inception/2
                        ,fun maybe_find_resource/2
                        ,fun maybe_set_ringback/2
                        ,fun maybe_set_transfer_media/2
                        ,fun maybe_lookup_cnam/2
                        ,fun relay_request/2
                        ,fun maybe_transition_port_in/2
                       ],
            lists:foldl(fun(F, J) ->  F(Props, J) end
                        ,JObj
                        ,Routines)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec set_account_id(wh_proplist(), wh_json:object()) -> wh_json:object().
set_account_id(Props, JObj) ->
    AccountId = props:get_value('account_id', Props),
    wh_json:set_value(?CCV(<<"Account-ID">>), AccountId, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_inception(wh_proplist(), wh_json:object()) -> wh_json:object().
set_inception(_, JObj) ->
    wh_json:set_value(?CCV(<<"Inception">>), <<"off-net">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_find_resource(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_find_resource(_, JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> JObj;
        {'ok', Props} ->
            case props:get_value('global', Props) of
                'true' ->
                    ResourceId = props:get_value('resource_id', Props),
                    wh_json:set_value(?CCV(<<"Resource-ID">>), ResourceId, JObj);
                'false' -> JObj
            end
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_ringback(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_set_ringback(Props, JObj) ->
    case props:get_value('ringback_media', Props) of
        'undefined' -> JObj;
        MediaId ->
            wh_json:set_value(?CCV(<<"Ringback-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec maybe_set_transfer_media(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_set_transfer_media(Props, JObj) ->
    case props:get_value('transfer_media', Props) of
        'undefined' -> JObj;
        MediaId ->
            wh_json:set_value(?CCV(<<"Transfer-Media">>), MediaId, JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec maybe_lookup_cnam(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_lookup_cnam(Props, JObj) ->
    case props:get_value('inbound_cnam', Props) of
        'false' -> JObj;
        'true' -> stepswitch_cnam:lookup(JObj)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_request(wh_proplist(), wh_json:object()) -> wh_json:object().
relay_request(_, JObj) ->
    wapi_route:publish_req(JObj).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_transition_port_in(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_transition_port_in(Props, _) ->
    case props:get_value('pending_port', Props) of
        'false' -> 'false';
        'true' ->
            Number = props:get_value('number', Props),
            wh_number_manager:ported(wnm_util:normalize_number(Number))
  end.
