%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2018, 2600Hz
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
    Bindings = bindings(),
    Responders = [{{?MODULE, 'handle_event'}
                  ,[{<<"notification">>, <<"*">>}]
                  }
                 ],
    {Bindings, Responders}.

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
-spec handle_event(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_event(JObj, _Props) ->
    kz_util:put_callid(JObj),

    EventName = kz_api:event_name(JObj),
    EventDefinition = kapi_notifications:api_definition(EventName),

    Validate = kapi_definition:validate_fun(EventDefinition),
    'true' = Validate(JObj),

    AccountId = kapi_notifications:account_id(JObj),
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle ~s for ~s", [EventName, AccountId]);
        Hooks ->
            Event = kz_json:normalize(JObj),
            Filtered = [Hook || Hook <- Hooks, match_action_type(Hook, EventName)],

            lager:debug("found ~b hook(s) to handle ~s", [length(Filtered), EventName]),
            webhooks_util:fire_hooks(Event, Filtered)
    end.

-spec match_action_type(webhook(), kz_term:api_binary()) -> boolean().
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data='undefined'
                          }, _Type) ->
    'true';
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data=JObj
                          }, Type) ->
    kz_json:get_value(<<"type">>, JObj) =:= Type
        orelse kz_json:get_value(<<"type">>, JObj) =:= <<"all">>;
match_action_type(#webhook{}=_W, _Type) ->
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
