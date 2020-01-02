%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_notifications).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_event/2
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"notifications">>).
-define(NAME, <<"Notifications Webhook">>).
-define(DESC, <<"Fire a webhook when a notification event is triggered in Kazoo">>).

-define(TYPE_ALL
       ,kz_json:from_list([{<<"friendly_name">>, <<"All">>}
                          ,{<<"description">>, <<"This event is triggered for any notification events">>}
                          ])
       ).

-define(ALL_TYPE_MODIFIERS(Modifiers), kz_json:from_list([{<<"all">>, ?TYPE_ALL} | Modifiers])).

-define(TYPE_MODIFIERS(Modifiers)
       ,kz_json:from_list([{<<"type">>, <<"object">>}
                          ,{<<"items">>, kz_json:merge(?ALL_TYPE_MODIFIERS(Modifiers), kz_json:new())} %% merge to sort :)
                          ])
       ).

-define(METADATA(Modifiers)
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ,{<<"modifiers">>
                           ,kz_json:from_list([{<<"type">>, ?TYPE_MODIFIERS(Modifiers)}])
                           }
                          ])
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    init(get_notifications_definition(), []).

-spec init(kapi_definition:apis(), [{kz_term:ne_binary(), kz_json:object()}]) -> 'ok'.
init([], Acc) ->
    _ = webhooks_util:init_metadata(?ID, ?METADATA(Acc)),
    'ok';
init([EventDefinition|Rest], Acc) ->
    Description = kz_json:from_list([{<<"friendly_name">>, kapi_definition:friendly_name(EventDefinition)}
                                    ,{<<"description">>, kapi_definition:description(EventDefinition)}
                                    ]
                                   ),
    init(Rest, [{kapi_definition:name(EventDefinition), Description} | Acc]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {bindings(), responders()}.

-spec bindings() -> gen_listener:bindings().
bindings() ->
    [{'notifications', [{'restrict_to'
                        ,[kapi_definition:restrict_to(Definition)
                          || Definition <- get_notifications_definition()
                         ]
                        }
                       ]
     }
    ].

-spec responders() -> gen_listener:responders().
responders() ->
    [{{?MODULE, 'handle_event'}
     ,[{<<"notification">>, <<"*">>}]
     }
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec account_bindings(kz_term:ne_binary()) -> gen_listener:bindings().
account_bindings(_AccountId) -> [].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kapi_notifications:doc(), kz_term:proplist()) -> 'ok'.
handle_event(Notification, _Props) ->
    kz_log:put_callid(Notification),

    EventName = kz_api:event_name(Notification),

    assert_valid(Notification, EventName),

    handle_account_notification(Notification, EventName).

handle_account_notification(Notification, <<"webhook">>) ->
    Hook = webhooks_util:from_json(kz_json:get_ne_json_value(<<"Hook">>, Notification, kz_json:new())),
    Data = kz_json:normalize(kz_json:get_ne_json_value(<<"Data">>, Notification, kz_json:new())),
    webhooks_util:fire_hooks(Data, [Hook]);
handle_account_notification(Notification, EventName) ->
    AccountId = kapi_notifications:account_id(Notification),
    lager:info("searching for hook ~s(~s) for account ~s", [EventName, ?HOOK_NAME, AccountId]),
    handle_account_notification(Notification, EventName, AccountId
                               ,webhooks_util:find_webhooks(?HOOK_NAME, AccountId)
                               ).

handle_account_notification(_Notification, _EventName, _AccountId, []) ->
    lager:debug("no hooks to handle ~s for ~s", [_EventName, _AccountId]);
handle_account_notification(Notification, EventName, _AccountId, Hooks) ->
    Event = kz_json:normalize(Notification),
    Filtered = [Hook || Hook <- Hooks, match_action_type(Hook, EventName)],

    lager:debug("found ~b hook(s) to handle ~s", [length(Filtered), EventName]),
    webhooks_util:fire_hooks(Event, Filtered).

-spec assert_valid(kapi_notifications:doc(), kz_term:ne_binary()) -> 'true'.
assert_valid(Notification, EventName) ->
    EventDefinition = kapi_notifications:api_definition(EventName),

    Validate = kapi_definition:validate_fun(EventDefinition),
    'true' = Validate(Notification).

-spec match_action_type(webhook(), kz_term:api_binary()) -> boolean().
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data='undefined'
                          }, _EventName) ->
    'true';
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data = CustomData
                          }, EventName) ->
    Type = kz_json:get_ne_binary_value(<<"type">>, CustomData),
    Type =:= EventName
        orelse Type =:= <<"all">>;
match_action_type(#webhook{}=_W, _EventName) ->
    'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_notifications_definition() -> kapi_definition:apis().
get_notifications_definition() ->
    [Definition
     || Definition <- kapi_notifications:api_definitions(),
        kapi_definition:name(Definition) =/= <<"skel">>,
        kapi_definition:name(Definition) =/= <<"notify_update">>
    ].
