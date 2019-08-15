%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Callflow action to control call waiting feature.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`action'</dt>
%%%   <dd>The action to be done: `activate', `deactivate' and `toggle'.
%%%   Default is `toggle'.</dd>
%%%
%%%   <dt>`scope'</dt>
%%%   <dd>Which endpoint this action must be set: `device', `user'.
%%%   Default is `user'.</dd>
%%% </dl>
%%%
%%%
%%% @author SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_call_waiting).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-record(call_waiting, {enabled = 'true' :: boolean()
                      ,jobj = kz_json:new() :: kz_json:object()
                      ,account_db :: kz_term:api_binary()
                      ,action :: kz_term:ne_binary()
                      }).
-type call_waiting() :: #call_waiting{}.

-type switch_fun() :: fun((boolean()) -> boolean()).
-spec actions() -> kz_term:proplist_kv(kz_term:ne_binary(), switch_fun()).
actions() ->
    [{<<"toggle">>, fun (Enabled) -> not Enabled end}
    ,{<<"activate">>, fun (_Enabled) -> 'true' end}
    ,{<<"deactivate">>, fun (_Enabled) -> 'false' end}
    ].

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case maybe_build_call_waiting_record(Data, Call) of
        {'error', _} ->
            lager:warning("unable to determine what document to apply call waiting against", []),
            _ = kapps_call_command:b_prompt(<<"cw-not_available">>, Call),
            cf_exe:stop(Call);
        {'ok', #call_waiting{} = CW} ->
            _ = kapps_call_command:answer(Call),
            _ = maybe_execute_action(CW, Call),
            cf_exe:continue(Call)
    end.

-spec maybe_build_call_waiting_record(kz_json:object(), kapps_call:call()) ->
                                             {'ok', call_waiting()} |
                                             {'error', any()}.
maybe_build_call_waiting_record(Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    DocId = get_doc_id(kz_json:get_ne_binary_value(<<"scope">>, Data, <<"device">>), Call),
    case maybe_get_doc(AccountDb, DocId) of
        {'error', _}= _E -> _E;
        {'ok', JObj} ->
            lager:info("changing call waiting settings on document ~s", [kz_doc:id(JObj)]),
            {'ok', #call_waiting{enabled = kz_json:is_true([<<"call_waiting">>, <<"enabled">>], JObj, 'true')
                                ,jobj = JObj
                                ,account_db = AccountDb
                                ,action = kz_json:get_ne_binary_value(<<"action">>, Data, <<"toggle">>)
                                }
            }
    end.

-spec get_doc_id(kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
get_doc_id(<<"user">>, Call) ->
    kapps_call:owner_id(Call);
get_doc_id(<<"device">>, Call) ->
    kapps_call:authorizing_id(Call).

-spec maybe_get_doc(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:jobj_return().
maybe_get_doc(_, 'undefined') ->
    {'error', 'no_device_id'};
maybe_get_doc('undefined', _) ->
    {'error', 'no_account_db'};
maybe_get_doc(AccountDb, Id) ->
    kz_datamgr:open_cache_doc(AccountDb, Id).

-spec maybe_execute_action(call_waiting(), kapps_call:call()) -> 'ok' | 'error'.
maybe_execute_action(#call_waiting{action = Action}=CW, Call) ->
    case props:get_value(Action, actions()) of
        'undefined' ->
            lager:info("unsupported call waiting action ~s", [Action]),
            kapps_call_command:b_prompt(<<"cw-not_available">>, Call);
        ActionFun -> execute_action(ActionFun, CW, Call)
    end.

-spec execute_action(switch_fun(), call_waiting(), kapps_call:call()) -> 'ok' | 'error'.
execute_action(ActionFun, #call_waiting{enabled = Enabled}=CW, Call) ->
    NewEnabled = ActionFun(Enabled),
    _ = play_related_media(NewEnabled, Call),
    maybe_update_doc(NewEnabled, CW).

-spec play_related_media(boolean(), kapps_call:call()) -> any().
play_related_media('true', Call) ->
    lager:debug("call waiting enabled"),
    kapps_call_command:b_prompt(<<"cw-activated">>, Call);
play_related_media('false', Call) ->
    lager:debug("call waiting disabled"),
    kapps_call_command:b_prompt(<<"cw-deactivated">>, Call).

-spec maybe_update_doc(boolean(), call_waiting()) -> 'ok' | 'error'.
maybe_update_doc(Enabled, CW) ->
    maybe_update_doc(Enabled, CW, 0).

-spec maybe_update_doc(boolean(), call_waiting(), 0..3) -> 'ok' | 'error'.
maybe_update_doc(Enabled, #call_waiting{enabled = Enabled}, _Retries) -> 'ok';
maybe_update_doc(Enabled
                ,#call_waiting{jobj = JObj
                              ,account_db = AccountDb
                              }=CW
                ,Retries
                ) ->
    Updated = kz_json:set_value([<<"call_waiting">>, <<"enabled">>], Enabled, JObj),
    case kz_datamgr:save_doc(AccountDb, Updated) of
        {'ok', _} ->
            lager:info("call_waiting.enabled set to ~s on ~s ~s"
                      ,[Enabled, kz_doc:type(JObj), kz_doc:id(JObj)]
                      );
        {'error', 'conflict'} ->
            retry_update_doc(Enabled, CW, Retries);
        {'error', _R} ->
            lager:debug("unable to update call_waiting.enabled on ~s ~s: ~p"
                       ,[kz_doc:type(JObj), kz_doc:id(JObj), _R]
                       ),
            'error'
    end.

-spec retry_update_doc(boolean(), call_waiting(), 0..3) -> 'ok' | 'error'.
retry_update_doc(_Enabled, #call_waiting{jobj = JObj}, Retries) when Retries >= 3 ->
    lager:info("to many attempts to update call_waiting for ~s ~s: ~p"
              ,[kz_doc:type(JObj), kz_doc:id(JObj)]
              ),
    'error';
retry_update_doc(Enabled
                ,#call_waiting{jobj = JObj
                              ,account_db = AccountDb
                              }=CW
                ,Retries
                ) ->
    case kz_datamgr:open_doc(AccountDb, kz_doc:id(JObj)) of
        {'ok', NewJObj} ->
            maybe_update_doc(Enabled, CW#call_waiting{jobj = NewJObj}, Retries+1);
        {'error', _R} ->
            lager:info("unable to retry call_waiting update for ~s ~s: ~p"
                      ,[kz_doc:type(JObj), kz_doc:id(JObj), _R]
                      ),
            'error'
    end.
