%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_fax).

-include("../webhooks.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, Props) ->
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    handle_event(JObj, Props, EventName).

-spec handle_event(api_binary(), wh_json:object(), wh_proplist()) -> 'ok'.
handle_event(JObj, _Props, <<"outbound_fax">> =EventName) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Formated = format_outbound_fax_event(JObj),
    maybe_send_event(EventName, AccountId, Formated);
handle_event(JObj, _Props, <<"outbound_fax_error">> =EventName) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Formated = format_outbound_fax_event(JObj),
    maybe_send_event(EventName, AccountId, Formated);
handle_event(_JObj, _Props, Event) ->
    lager:error("received unhandle message ~p", [Event]).

-spec maybe_send_event(ne_binary(), ne_binary(), wh_json:object()) -> 'ok'.
maybe_send_event(EventName, AccountId, JObj) ->
    case webhooks_util:find_webhooks(EventName, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [EventName, AccountId]);
        Hooks -> webhooks_util:fire_hooks(JObj, Hooks)
    end.

-spec format_outbound_fax_event(wh_json:object()) -> wh_json:object().
format_outbound_fax_event(JObj) ->
    RemoveKeys = [<<"Fax-Notifications">>
                  ,<<"Node">>
                  ,<<"Msg-ID">>
                  ,<<"App-Version">>
                  ,<<"App-Name">>
                  ,<<"Event-Category">>
                  ,<<"Fax-Info">>
                 ],
    FaxInfo = wh_util:clean_jobj(wh_json:get_value(<<"Fax-Info">>, JObj, wh_json:new())
                         ,[]
                         ,[]),
    wh_json:merge_jobjs(FaxInfo
                        ,wh_util:clean_jobj(JObj, RemoveKeys, [])).


