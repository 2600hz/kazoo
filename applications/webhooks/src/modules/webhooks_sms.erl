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
-spec handle_sms(kapi_conf:doc(), kz_term:proplist()) -> 'ok'.
handle_sms(ConfChange, _Props) ->
    kz_log:put_callid(ConfChange),
    'true' = kapi_conf:doc_update_v(ConfChange),
    Type = kapi_conf:get_type(ConfChange),
    Action = kz_api:event_name(ConfChange),
    handle_sms(Action, Type, ConfChange).

-spec handle_sms(kz_term:api_ne_binary(), kz_term:api_ne_binary(), kapi_conf:doc()) -> 'ok'.
handle_sms(?DOC_CREATED, <<"sms">>, ConfChange) ->
    Db = kapi_conf:get_database(ConfChange),

    case kz_util:format_account_id(Db) of
        'undefined' -> 'ok';
        AccountId ->
            handle_account_sms(ConfChange, Db, AccountId)
    end;
handle_sms(_Action, _Type, _ConfChange) -> 'ok'.

-spec handle_account_sms(kapi_conf:doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_account_sms(ConfChange, Db, AccountId) ->
    Id = kapi_conf:get_id(ConfChange),
    {'ok', SMSDoc} = kz_datamgr:open_cache_doc(Db, {kzd_sms:type(), Id}),

    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle ~s for ~s", [kz_api:event_name(ConfChange), AccountId]);
        Hooks ->
            Event = format_event(SMSDoc, AccountId),
            webhooks_util:fire_hooks(Event, Hooks)
    end.

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
-spec format_event(kzd_sms:doc(), kz_term:ne_binary()) -> kz_json:object().
format_event(SMSDoc, AccountId) ->
    kz_json:from_list(
      [{<<"id">>, kz_doc:id(SMSDoc)}
      ,{<<"account_id">>, AccountId}
      ,{<<"from">>, kzd_sms:from_user(SMSDoc)}
      ,{<<"to">>, kzd_sms:to_user(SMSDoc)}
      ,{<<"body">>, kzd_sms:body(SMSDoc)}
      ,{<<"direction">>, kzd_sms:direction(SMSDoc)}
      ,{<<"status">>, kzd_sms:status(SMSDoc)}
      ]).
