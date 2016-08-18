%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_object).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_event/2
        ]).

-include("webhooks.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-define(ID, kz_util:to_binary(?MODULE)).
-define(NAME, <<"object">>).
-define(DESC, <<"Receive notifications when objects in Kazoo are changed">>).

-define(
   OBJECT_TYPES
       ,kapps_config:get(
          ?APP_NAME
                        ,<<"object_types">>
                        ,?DOC_TYPES
         )
  ).

-define(
   TYPE_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of object types to handle">>}
          ,{<<"items">>, ?OBJECT_TYPES}
          ])
  ).

-define(
   ACTIONS_MODIFIER
       ,kz_json:from_list(
          [{<<"type">>, <<"array">>}
          ,{<<"description">>, <<"A list of object actions to handle">>}
          ,{<<"items">>, ?DOC_ACTIONS}
          ])
  ).

-define(
   MODIFIERS
       ,kz_json:from_list(
          [{<<"type">>, ?TYPE_MODIFIER}
          ,{<<"action">>, ?ACTIONS_MODIFIER}
          ])
  ).

-define(
   METADATA
       ,kz_json:from_list(
          [{<<"_id">>, ?ID}
          ,{<<"name">>, ?NAME}
          ,{<<"description">>, ?DESC}
          ,{<<"modifiers">>, ?MODIFIERS}
          ])
  ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    Bindings = bindings(load_accounts()),
    Responders = [{{?MODULE, 'handle_event'}, [{<<"configuration">>, <<"*">>}]}],
    {Bindings, Responders}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec account_bindings(ne_binary()) -> gen_listener:bindings().
account_bindings(AccountId) ->
    bindings([kz_util:format_account_id(AccountId, 'encoded')]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_proplist()) -> any().
handle_event(JObj, _Props) ->
    kz_util:put_callid(JObj),
    'true' = kapi_conf:doc_update_v(JObj),

    AccountId = find_account_id(JObj),
    case webhooks_util:find_webhooks(?NAME, AccountId) of
        [] ->
            lager:debug(
              "no hooks to handle ~s for ~s"
                       ,[kz_api:event_name(JObj), AccountId]
             );
        Hooks ->
            webhooks_util:fire_hooks(format_event(JObj, AccountId), Hooks)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_accounts() -> ne_binaries().
load_accounts() ->
    case
        kz_datamgr:get_results(
          ?KZ_WEBHOOKS_DB
                              ,<<"webhooks/hook_listing">>
                              ,[{'key', ?NAME}]
         )
    of
        {'ok', View} ->
            [kz_util:format_account_id(
               kz_json:get_value(<<"value">>, Result)
                                      ,'encoded'
              )
             || Result <- View
            ];
        {'error', _E} ->
            lager:warning("failed to load accounts: ~p", [_E]),
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec bindings(ne_binaries()) -> gen_listener:bindings().
bindings([]) ->
    lager:debug("no accounts configured"),
    [];
bindings(AccountsWithObjectHook) ->
    [{'conf'
     ,[{'restrict_to', ['doc_updates']}
      ,{'type', Type}
      ,{'db', Account}
      ,'federate'
      ]
     }
     || Type <- ?OBJECT_TYPES,
        Account <- lists:usort(AccountsWithObjectHook)
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format_event(kz_json:object(), ne_binary()) -> kz_json:object().
format_event(JObj, AccountId) ->
    kz_json:from_list(
      props:filter_undefined(
        [{<<"id">>, kapi_conf:get_id(JObj)}
        ,{<<"account_id">>, AccountId}
        ,{<<"action">>, kz_api:event_name(JObj)}
        ,{<<"type">>, kapi_conf:get_type(JObj)}
        ])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_account_id(kz_json:object()) -> ne_binary().
find_account_id(JObj) ->
    case kapi_conf:get_account_id(JObj) of
        'undefined' ->
            kz_util:format_account_id(kapi_conf:get_account_db(JObj), 'raw');
        AccountId -> AccountId
    end.
