%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%% Handle route requests
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_inbound).

-export([init/0, handle_req/2]).

-include("stepswitch.hrl").

-spec init() -> 'ok'.
init() -> 'ok'.

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Prop) ->
    _ = whapps_util:put_callid(JObj),
    case wh_json:get_ne_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        'undefined' ->
            lager:debug("received new inbound dialplan route request"),
            _ =  inbound_handler(JObj);
        _AcctID -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle a request inbound from offnet
%% @end
%%--------------------------------------------------------------------
-spec inbound_handler(wh_json:object()) -> 'ok'.
-spec inbound_handler(wh_json:object(), ne_binary()) -> 'ok'.
inbound_handler(JObj) ->
    inbound_handler(JObj, get_dest_number(JObj)).
inbound_handler(JObj, Number) ->
    case stepswitch_util:lookup_number(Number) of
        {'ok', AccountId, Props} ->
            lager:debug("number associated with account ~s", [AccountId]),
            relay_route_req(AccountId, Props, JObj);
        {'error', _R} ->
            lager:debug("failed to find account for number ~s: ~p", [Number, _R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec get_dest_number(wh_json:object()) -> ne_binary().
get_dest_number(JObj) ->
    {User, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case whapps_config:get_is_true(<<"stepswitch">>, <<"assume_inbound_e164">>) of
        'true' ->
            Number = assume_e164(User),
            lager:debug("assuming number is e164, normalizing to ~s", [Number]),
            Number;
        _ ->
            Number = wnm_util:to_e164(User),
            lager:debug("converted number to e164: ~s", [Number]),
            Number
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec assume_e164(ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) -> Number;
assume_e164(Number) -> <<$+, Number/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec custom_channel_vars(ne_binary(), wh_json:object()) -> wh_json:object().
custom_channel_vars(AccountId, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, wh_json:new()),
    RemoveKeys = [<<"Account-ID">>
                  ,<<"Inception">>
                  ,<<"Authorizing-ID">>
                 ],
    Props = [{<<"Account-ID">>, AccountId}
             ,{<<"Inception">>, <<"off-net">>}
            ],
    UpdatedCCVs = wh_json:set_values(Props, wh_json:delete_keys(RemoveKeys, CCVs)),
    wh_json:set_value(<<"Custom-Channel-Vars">>, UpdatedCCVs, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_route_req(ne_binary(), wh_proplist(), wh_json:object()) -> 'ok'.
relay_route_req(AccountId, Props, JObj) ->
    Routines = [fun(J) -> custom_channel_vars(AccountId, J) end
                ,fun(J) ->
                         case props:get_value('inbound_cnam', Props) of
                             'false' -> J;
                             'true' -> stepswitch_cnam:lookup(J)
                         end
                 end
                ,fun(J) ->
                         case props:get_value('ringback_media', Props) of
                             'undefined' -> J;
                             MediaId ->
                                 wh_json:set_value(<<"Ringback-Media">>, MediaId, J)
                         end
                 end
                ,fun(J) ->
                         case props:get_value('transfer_media', Props) of
                             'undefined' -> J;
                             MediaId ->
                                 wh_json:set_value(<<"Transfer-Media">>, MediaId, J)
                         end
                 end
               ],
    wapi_route:publish_req(lists:foldl(fun(F, J) -> F(J) end, JObj, Routines)),
    lager:debug("relayed route request").
