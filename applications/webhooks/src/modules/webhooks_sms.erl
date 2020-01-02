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
-module(webhooks_sms).

-export([init/0
        ,bindings_and_responders/0
        ,account_bindings/1
        ,handle_sms/2
        ]).

-include("webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"sms">>).
-define(NAME, <<"SMS">>).
-define(DESC, <<"Receive notifications when sms is received">>).

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
    Responders = [{{?MODULE, 'handle_sms'}, [{<<"sms">>, <<"inbound">>}]}],
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
-spec handle_sms(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_sms(Payload, _Props) ->
    kz_log:put_callid(Payload),
    'true' = kapi_im:inbound_v(Payload),
    AccountId = kz_im:account_id(Payload),
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] ->
            lager:debug("no hooks to handle ~s for ~s", [kz_api:event_name(Payload), AccountId]);
        Hooks ->
            Event = format_event(Payload, AccountId),
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
    [{'im', [{'restrict_to', ['inbound']}
            ,{'route_type', 'offnet'}
            ,{'im_types', ['sms']}
            ]
     }
    ].
%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format_event(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
format_event(Payload, AccountId) ->
    kz_json:from_list(
      [{<<"id">>, kz_im:message_id(Payload)}
      ,{<<"account_id">>, AccountId}
      ,{<<"type">>, kz_im:type(Payload)}
      ,{<<"from">>, kz_im:from(Payload)}
      ,{<<"to">>, kz_im:to(Payload)}
      ,{<<"body">>, kz_im:body(Payload)}
      ,{<<"origin">>, kz_im:route_type(Payload)}
      ]).
