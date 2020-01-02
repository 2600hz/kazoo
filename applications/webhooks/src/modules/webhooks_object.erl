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
-module(webhooks_object).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_event/2
        ]).

-include("webhooks.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/doc_types.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"object">>).
-define(NAME, <<"Object">>).
-define(DESC, <<"Receive notifications when objects (like JSON document objects) in Kazoo are changed">>).

-define(OBJECT_TYPES
       ,kapps_config:get(?APP_NAME, <<"object_types">>, ?DOC_TYPES)
       ).

-define(TYPE_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of object types to handle">>}
          ,{<<"items">>, [<<"all">> | ?OBJECT_TYPES]}
          ]
         )
       ).

-define(ACTIONS_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of object actions to handle">>}
          ,{<<"items">>, [<<"all">> | ?DOC_ACTIONS]}
          ]
         )
       ).

-define(MODIFIERS
       ,kz_json:from_list(
          [{<<"type">>, ?TYPE_MODIFIER}
          ,{<<"action">>, ?ACTIONS_MODIFIER}
          ]
         )
       ).

-define(METADATA
       ,kz_json:from_list(
          [{<<"_id">>, ?ID}
          ,{<<"name">>, ?NAME}
          ,{<<"description">>, ?DESC}
          ,{<<"modifiers">>, ?MODIFIERS}
          ]
         )
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {bindings(), responders()}.

-spec responders() -> gen_listener:responders().
responders() ->
    [{{?MODULE, 'handle_event'}
     ,[{<<"configuration">>, <<"*">>}]
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
-spec handle_event(kapi_conf:doc(), kz_term:proplist()) -> 'ok'.
handle_event(DocChange, _Props) ->
    kz_log:put_callid(DocChange),
    'true' = kapi_conf:doc_update_v(DocChange),

    case find_account_id(DocChange) of
        'undefined' -> 'ok';
        AccountId ->
            handle_account_change(DocChange, AccountId)
    end.

-spec handle_account_change(kapi_conf:doc(), kz_term:ne_binary()) -> 'ok'.
handle_account_change(DocChange, AccountId) ->
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle ~s(~s) for ~s", [?HOOK_NAME, kz_api:event_name(DocChange), AccountId]);
        Hooks ->
            EventJObj = format_event(DocChange, AccountId),
            Action = kz_api:event_name(DocChange),
            Type = kapi_conf:get_type(DocChange),

            lager:debug("event for action ~s type ~s", [Action, Type]),

            Filtered = [Hook || Hook <- Hooks, match_action_type(Hook, Action, Type)],
            webhooks_util:fire_hooks(EventJObj, Filtered)
    end.

-spec match_action_type(webhook(), kz_term:api_binary(), kz_term:api_binary()) -> boolean().
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data='undefined'
                          }, _Action, _Type) ->
    'true';
match_action_type(#webhook{hook_event = ?HOOK_NAME
                          ,custom_data=CustomData
                          }, Action, Type) ->
    DataAction = kz_json:get_ne_binary_value(<<"action">>, CustomData),
    DataType = kz_json:get_ne_binary_value(<<"type">>, CustomData),

    match_action_type(DataAction, Action, DataType, Type);
match_action_type(#webhook{}, _Action, _Type) ->
    'true'.

-spec match_action_type(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
match_action_type(<<"all">>, _Action, <<"all">>, _Type) ->
    lager:debug("hook matches all actions and all types"),
    'true';
match_action_type(<<"all">>, _Action, Type, Type) ->
    lager:debug("hook matches all actions and type ~s", [Type]),
    'true';
match_action_type(Action, Action, <<"all">>, _Type) ->
    lager:debug("hook matches action ~s and all types", [Action]),
    'true';
match_action_type(Action, Action, Type, Type) ->
    lager:debug("hook matches action ~s and type ~s", [Action, Type]),
    'true';
match_action_type(_DataAction, _EventAction, _DataType, _EventType) ->
    lager:debug("hook action ~s =/= ~s and type ~s =/= ~s", [_DataAction, _EventAction, _DataType, _EventType]),
    'false'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bindings() -> gen_listener:bindings().
bindings() ->
    [{'conf', [{'restrict_to', ['doc_updates']}]}].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_event(kapi_conf:doc(), kz_term:ne_binary()) -> kz_json:object().
format_event(ConfChange, AccountId) ->
    kz_json:from_list(
      [{<<"id">>, kapi_conf:get_id(ConfChange)}
      ,{<<"account_id">>, AccountId}
      ,{<<"action">>, kz_api:event_name(ConfChange)}
      ,{<<"type">>, kapi_conf:get_type(ConfChange)}
      ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account_id(kapi_conf:doc()) -> kz_term:api_ne_binary().
find_account_id(ConfChange) ->
    DB = kapi_conf:get_database(ConfChange),
    find_account_id(kzs_util:db_classification(DB), DB, kapi_conf:get_id(ConfChange)).

-spec find_account_id(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
find_account_id(Classification, DB, _Id)
  when Classification =:= 'account';
       Classification =:= 'modb' ->
    kzs_util:format_account_id(DB);
find_account_id('aggregate', ?KZ_ACCOUNTS_DB, Id) -> Id;
find_account_id(_, _, _) -> 'undefined'.
