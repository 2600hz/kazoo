%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_outbound_fax).

-export([init/0
         ,bindings_and_responders/0
         ,handle_event/2
        ]).

-include("../webhooks.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"outbound_fax">>).
-define(DESC, <<"Outbound faxes (or errors if they occur)">>).
-define(METADATA
        ,wh_json:from_list([{<<"_id">>, ?ID}
                            ,{<<"name">>, ?NAME}
                            ,{<<"description">>, ?DESC}
                           ])
       ).

-define(FAX_NOTIFY_RESTRICT_TO, ['outbound_fax'
                                 ,'outbound_fax_error'
                                ]).

-define(BINDINGS, [{'notifications'
                    ,[{'restrict_to', ?FAX_NOTIFY_RESTRICT_TO}]
                   }
                  ]
       ).
-define(RESPONDERS
        ,[{{'webhooks_outbound_fax', 'handle_event'}
           ,[{<<"notification">>, ?NAME}
             ,{<<"notification">>, <<"outbound_fax_error">>}
            ]
          }
         ]
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() ->
                                     {gen_listener:bindings()
                                      ,gen_listener:responders()
                                     }.
bindings_and_responders() ->
    {?BINDINGS, ?RESPONDERS}.

-spec handle_event(wh_json:object(), wh_proplist()) -> 'ok'.
-spec handle_event(wh_json:object(), wh_proplist(), ne_binary()) -> 'ok'.
handle_event(JObj, Props) ->
    EventName = wh_json:get_value(<<"Event-Name">>, JObj),
    handle_event(JObj, Props, EventName).

handle_event(JObj, _Props, ?NAME = EventName) ->
    'true' = wapi_notifications:fax_outbound_v(JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Formatted = format_outbound_fax_event(JObj),
    maybe_send_event(EventName, AccountId, Formatted);
handle_event(JObj, _Props, <<"outbound_fax_error">>) ->
    'true' = wapi_notifications:fax_outbound_error_v(JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    Formatted = format_outbound_fax_event(JObj),
    maybe_send_event(?NAME, AccountId, Formatted).

-spec maybe_send_event(ne_binary(), api_binary(), wh_json:object()) -> 'ok'.
maybe_send_event(_EventName, 'undefined', _JObj) -> 'ok';
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
    FaxInfo = wh_json:normalize_jobj(wh_json:get_value(<<"Fax-Info">>, JObj, wh_json:new())),
    wh_json:merge_jobjs(FaxInfo
                        ,wh_json:normalize_jobj(JObj, RemoveKeys, [])
                       ).
