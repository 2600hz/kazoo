%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_object).

-export([init/0
         ,bindings_and_responders/0
         ,account_bindings/1
         ,handle_event/2
        ]).

-include("../webhooks.hrl").
-include_lib("whistle/include/wapi_conf.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"object">>).
-define(DESC, <<"Receive notifications when objects in Kazoo are changed">>).

-define(
   OBJECT_TYPES
   ,whapps_config:get(
      ?APP_NAME
      ,<<"object_types">>
      ,?DOC_TYPES
     )
  ).

-define(
   TYPE_MODIFIER
   ,wh_json:from_list(
      [{<<"type">>, <<"array">>}
       ,{<<"description">>, <<"A list of object types to handle">>}
       ,{<<"items">>, ?OBJECT_TYPES}
      ])
  ).

-define(
   ACTIONS_MODIFIER
   ,wh_json:from_list(
      [{<<"type">>, <<"array">>}
       ,{<<"description">>, <<"A list of object actions to handle">>}
       ,{<<"items">>, ?DOC_ACTIONS}
      ])
  ).

-define(
   MODIFIERS
   ,wh_json:from_list(
      [{<<"type">>, ?TYPE_MODIFIER}
       ,{<<"action">>, ?ACTIONS_MODIFIER}
      ])
  ).

-define(
   METADATA
   ,wh_json:from_list(
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
    bindings([wh_util:format_account_id(AccountId, 'encoded')]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle_event(wh_json:object(), wh_proplist()) -> any().
handle_event(JObj, _Props) ->
    wh_util:put_callid(JObj),
    'true' = wapi_conf:doc_update_v(JObj),

    AccountId = find_account_id(JObj),
    case webhooks_util:find_webhooks(?NAME, AccountId) of
        [] ->
            lager:debug(
              "no hooks to handle ~s for ~s"
              ,[wh_api:event_name(JObj), AccountId]
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
        couch_mgr:get_results(
          ?KZ_WEBHOOKS_DB
          ,<<"webhooks/hook_listing">>
          ,[{'key', ?NAME}]
         )
    of
        {'ok', View} ->
            [wh_util:format_account_id(
               wh_json:get_value(<<"value">>, Result)
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
-spec format_event(wh_json:object(), ne_binary()) -> wh_json:object().
format_event(JObj, AccountId) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"id">>, wapi_conf:get_id(JObj)}
         ,{<<"account_id">>, AccountId}
         ,{<<"action">>, wh_api:event_name(JObj)}
         ,{<<"type">>, wapi_conf:get_type(JObj)}
        ])
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_account_id(wh_json:object()) -> ne_binary().
find_account_id(JObj) ->
    case wapi_conf:get_account_id(JObj) of
        'undefined' ->
            wh_util:format_account_id(wapi_conf:get_account_db(JObj), 'raw');
        AccountId -> AccountId
    end.
