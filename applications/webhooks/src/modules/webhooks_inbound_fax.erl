%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz INC
%%%
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(webhooks_inbound_fax).

-export([init/0
        ,bindings_and_responders/0
        ,handle_event/2
        ]).

-include("webhooks.hrl").

-define(ID, kz_util:to_binary(?MODULE)).
-define(NAME, <<"inbound_fax">>).
-define(DESC, <<"Inbound faxes (or errors if they occur)">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ])
       ).

-define(FAX_NOTIFY_RESTRICT_TO, ['inbound_fax'
                                ,'inbound_fax_error'
                                ]).

-define(BINDINGS, [{'notifications'
                   ,[{'restrict_to', ?FAX_NOTIFY_RESTRICT_TO}]
                   }
                  ]
       ).
-define(RESPONDERS
       ,[{{'webhooks_inbound_fax', 'handle_event'}
         ,[{<<"notification">>, ?NAME}
          ,{<<"notification">>, <<"inbound_fax_error">>}
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

-spec handle_event(kz_json:object(), kz_proplist()) -> 'ok'.
-spec handle_event(kz_json:object(), kz_proplist(), ne_binary()) -> 'ok'.
handle_event(JObj, Props) ->
    EventName = kz_json:get_value(<<"Event-Name">>, JObj),
    handle_event(JObj, Props, EventName).

handle_event(JObj, _Props, ?NAME = EventName) ->
    'true' = kapi_notifications:fax_inbound_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    Formatted = format_inbound_fax_event(JObj),
    maybe_send_event(EventName, AccountId, Formatted);
handle_event(JObj, _Props, <<"inbound_fax_error">>) ->
    'true' = kapi_notifications:fax_inbound_error_v(JObj),
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    Formatted = format_inbound_fax_event(JObj),
    maybe_send_event(?NAME, AccountId, Formatted).

-spec maybe_send_event(ne_binary(), api_binary(), kz_json:object()) -> 'ok'.
maybe_send_event(_EventName, 'undefined', _JObj) -> 'ok';
maybe_send_event(EventName, AccountId, JObj) ->
    case webhooks_util:find_webhooks(EventName, AccountId) of
        [] -> lager:debug("no hooks to handle ~s for ~s", [EventName, AccountId]);
        Hooks -> webhooks_util:fire_hooks(JObj, Hooks)
    end.

-spec format_inbound_fax_event(kz_json:object()) -> kz_json:object().
format_inbound_fax_event(JObj) ->
    RemoveKeys = [<<"Fax-Notifications">>
                 ,<<"Node">>
                 ,<<"Msg-ID">>
                 ,<<"App-Version">>
                 ,<<"App-Name">>
                 ,<<"Event-Category">>
                 ,<<"Fax-Info">>
                 ],
    FaxInfo = kz_json:normalize_jobj(kz_json:get_value(<<"Fax-Info">>, JObj, kz_json:new())),
    kz_json:merge_jobjs(FaxInfo
                       ,kz_json:normalize_jobj(JObj, RemoveKeys, [])
                       ).
