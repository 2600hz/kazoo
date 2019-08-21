%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%%
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_sms).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_sms/2
        ]).

-include("webhooks.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").
-include_lib("kazoo_documents/include/doc_types.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"sms">>).
-define(NAME, <<"SMS">>).
-define(DESC, <<"Receive notifications when sms is created">>).

-define(METADATA
       ,kz_json:from_list(
          [{<<"_id">>, ?ID}
          ,{<<"name">>, ?NAME}
          ,{<<"description">>, ?DESC}
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
    Bindings = bindings(),
    Responders = [{{?MODULE, 'handle_sms'}, [{<<"configuration">>, ?DOC_CREATED}]}],
    {Bindings, Responders}.

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
-spec handle_sms(kz_json:object(), kz_term:proplist()) -> any().
handle_sms(JObj, _Props) ->
    kz_util:put_callid(JObj),
    'true' = kapi_conf:doc_update_v(JObj),
    Type = kapi_conf:get_type(JObj),
    Action = kz_api:event_name(JObj),
    handle_sms(Action, Type, JObj).

-spec handle_sms(kz_term:api_ne_binary(), kz_term:api_ne_binary(), json:object()) -> any().
handle_sms(?DOC_CREATED, <<"sms">>, JObj) ->
    Db = kapi_conf:get_database(JObj),
    Id = kapi_conf:get_id(JObj),
    {'ok', Doc} = kz_datamgr:open_doc(Db, Id),
    AccountId = kz_util:format_account_id(Db),
    case AccountId =/= 'undefined'
        andalso webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        'false' -> 'ok';
        [] ->
            lager:debug("no hooks to handle ~s for ~s"
                       ,[kz_api:event_name(JObj), AccountId]
                       );
        Hooks ->
            Event = format_event(Doc, AccountId),
            webhooks_util:fire_hooks(Event, Hooks)
    end;
handle_sms(_Action, _Type, _JObj) ->
    'ok'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bindings() -> gen_listener:bindings().
bindings() ->
    [{'conf', [{'restrict_to', ['doc_updates']}
              ,{'action', 'created'}
              ,{'doc_type', <<"sms">>}
              ]}].
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_event(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
format_event(JObj, AccountId) ->
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(JObj)}
      ,{<<"account_id">>, AccountId}
      ,{<<"from">>, kzd_sms:from_user(JObj)}
      ,{<<"to">>, kzd_sms:to_user(JObj)}
      ,{<<"body">>, kzd_sms:body(JObj)}
      ,{<<"direction">>, kzd_sms:direction(JObj)}
      ,{<<"status">>, kzd_sms:status(JObj)}
      ]).
