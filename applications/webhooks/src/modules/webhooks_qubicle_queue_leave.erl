%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%%
%%%
%%% @author Mark Magnusson
%%% @end
%%%-----------------------------------------------------------------------------
-module(webhooks_qubicle_queue_leave).

-export([init/0
        ,bindings_and_responders/0
        ,handle/2
        ]).

-include_lib("webhooks/src/webhooks.hrl").

-define(ID, kz_term:to_binary(?MODULE)).
-define(HOOK_NAME, <<"qubicle_queue_leave">>).
-define(NAME, <<"Qubicle Queue Leave">>).
-define(DESC, <<"Fires when a session leaves a queue">>).
-define(METADATA
       ,kz_json:from_list([{<<"_id">>, ?ID}
                          ,{<<"name">>, ?NAME}
                          ,{<<"description">>, ?DESC}
                          ])
       ).

-spec init() -> 'ok'.
init() ->
    webhooks_util:init_metadata(?ID, ?METADATA).

-spec bindings_and_responders() -> {gen_listener:bindings(), gen_listener:responders()}.
bindings_and_responders() ->
    {[{'qubicle_queue', ['federate', 'event_only']}
     ]
    ,[{{?MODULE, 'handle'}
      ,[{<<"qubicle-queue">>, <<"leave">>}
       ]
      }
     ]
    }.

-spec handle(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle(JObj, _Props) ->
    AccountId = kz_json:get_value(<<"Account-ID">>, JObj),
    maybe_send_event(AccountId, format(JObj)).

-spec maybe_send_event(kz_term:api_binary(), kz_json:object()) -> 'ok'.
maybe_send_event('undefined', _JObj) -> 'ok';
maybe_send_event(AccountId, JObj) ->
    case webhooks_util:find_webhooks(?HOOK_NAME, AccountId) of
        [] -> lager:debug("no hooks to handle for ~s", [AccountId]);
        Hooks -> webhooks_util:fire_hooks(JObj, Hooks)
    end.

-spec format(kz_json:object()) -> kz_json:object().
format(JObj) ->
    RemoveKeys = [<<"Node">>
                 ,<<"Msg-ID">>
                 ,<<"App-Version">>
                 ,<<"App-Name">>
                 ,<<"Event-Category">>
                 ],
    kz_json:normalize_jobj(JObj, RemoveKeys, []).
