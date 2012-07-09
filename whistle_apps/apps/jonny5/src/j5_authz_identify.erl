%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_authz_identify).

-export([handle_req/2]).

-include("jonny5.hrl").

-define(IDENT_KEY(Key), {?MODULE, Key}).

-spec handle_req/2 :: (wh_json:json_object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    true = wapi_authz:identify_req_v(JObj),
    wh_util:put_callid(JObj),
    Number = get_dest_number(JObj),
    case wh_cache:peek_local(?JONNY5_CACHE, ?IDENT_KEY(Number)) of
        {ok, {AccountId, GlobalResource}} ->
            send_resp(JObj, AccountId, GlobalResource);
        {error, not_found} ->
            case wh_number_manager:lookup_account_by_number(Number) of
                {error, _} -> ok;
                {ok, AccountId, _, GlobalResource} ->
                    wh_cache:store_local(?JONNY5_CACHE, ?IDENT_KEY(Number), {AccountId, GlobalResource}),
                    send_resp(JObj, AccountId, GlobalResource)
            end
    end.

-spec get_dest_number/1 :: (wh_json:json_object()) -> ne_binary().
get_dest_number(JObj) ->
    {User, _} = whapps_util:get_destination(JObj, ?APP_NAME, <<"inbound_user_field">>),
    case whapps_config:get_is_true(?APP_NAME, <<"assume_inbound_e164">>, false) of
        true ->
            Number = assume_e164(User),
            lager:debug("assuming number is e164, normalizing to ~s", [Number]),
            Number;
        _ ->
            Number = wnm_util:to_e164(User),
            lager:debug("converted number to e164: ~s", [Number]),
            Number
    end.

-spec assume_e164/1 :: (ne_binary()) -> ne_binary().
assume_e164(<<$+, _/binary>> = Number) ->
    Number;
assume_e164(Number) ->
    <<$+, Number/binary>>.

-spec send_resp/3 :: (wh_json:json_object(), 'undefined' | ne_binary(), boolean()) -> 'ok'.
send_resp(JObj, AccountId, GlobalResource) ->
    lager:debug("channel identified as account ~s", [AccountId]),
    Resp = [{<<"Account-ID">>, AccountId}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Global-Resource">>, wh_util:to_binary(GlobalResource)}
            ,{<<"Reseller-ID">>, wh_reseller:find_reseller_id(AccountId)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_identify_resp(wh_json:get_value(<<"Server-ID">>, JObj), props:filter_undefined(Resp)).
