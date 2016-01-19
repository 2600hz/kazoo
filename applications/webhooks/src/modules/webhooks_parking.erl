%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------

-module(webhooks_parking).

-export([init/0
         ,bindings_and_responders/0
         ,handle/2
        ]).

-include("webhooks.hrl").

-define(ID, wh_util:to_binary(?MODULE)).
-define(NAME, <<"parking">>).
-define(DESC, <<"Events when calls get parked/retrieved">>).
-define(METADATA
        ,wh_json:from_list([{<<"_id">>, ?ID}
                            ,{<<"name">>, ?NAME}
                            ,{<<"description">>, ?DESC}
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
    {[{'call', [{'restrict_to', ['PARK_PARKED', 'PARK_RETRIEVED', 'PARK_ABANDONED']}
                ,'federate'
               ]
      }
     ]
     ,[{{?MODULE, 'handle'}
        ,[{<<"call_event">>, <<"PARK_PARKED">>}
          ,{<<"call_event">>, <<"PARK_RETRIEVED">>}
          ,{<<"call_event">>, <<"PARK_ABANDONED">>}]
       }
      ]
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
handle(JObj, _Props) ->
    'true' = wapi_call:event_v(JObj),
    AccountId = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj),
    maybe_send_event(AccountId, format(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_event(api_binary(), wh_json:object()) -> 'ok'.
maybe_send_event('undefined', _JObj) -> 'ok';
maybe_send_event(AccountId, JObj) ->
    case webhooks_util:find_webhooks(?NAME, AccountId) of
        [] -> lager:debug("no hooks to handle for ~s", [AccountId]);
        Hooks -> webhooks_util:fire_hooks(JObj, Hooks)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec format(wh_json:object()) -> wh_json:object().
format(JObj) ->
    RemoveKeys = [
        <<"Node">>
        ,<<"Msg-ID">>
        ,<<"App-Version">>
        ,<<"App-Name">>
        ,<<"Event-Category">>
        ,<<"Custom-Channel-Vars">>
    ],
    wh_json:normalize_jobj(JObj, RemoveKeys, []).