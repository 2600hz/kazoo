%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz INC
%%% @doc
%%%
%%% "data": {
%%%   "action": "activate" | "deactivate" | *"toggle",
%%%   "scope": "device" | *"user"
%%% }
%%%
%%% * Default value
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Maksim Krzhemenevskiy)
%%%-------------------------------------------------------------------
-module(cf_call_waiting).

-include("../callflow.hrl").

-export([handle/2]).

-record(call_waiting, {enabled = 'true' :: boolean()
                       ,jobj = wh_json:new() :: wh_json:object()
                       ,account_db :: api_binary()
                       ,action :: ne_binary()
                      }).
-type call_waiting() :: #call_waiting{}.

-type switch_fun() :: fun((boolean()) -> boolean()).
-spec actions() -> wh_proplist_kv(ne_binary(), switch_fun()).
actions() ->
    [{<<"toggle">>, fun (Enabled) -> not Enabled end}
     ,{<<"activate">>, fun (_Enabled) -> 'true' end}
     ,{<<"deactivate">>, fun (_Enabled) -> 'false' end}
    ].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case maybe_build_call_waiting_record(Data, Call) of
        {'error', _} ->
            lager:warning("unable to determine what document to apply call waiting against", []),
            _ = whapps_call_command:b_prompt(<<"cw-not_available">>, Call),
            cf_exe:stop(Call);
        {'ok', #call_waiting{} = CW} ->
            _ = whapps_call_command:answer(Call),
            _ = maybe_execute_action(CW, Call),
            cf_exe:continue(Call)
    end.

-spec maybe_build_call_waiting_record(wh_json:object(), whapps_call:call()) ->
                                    {'ok', call_waiting()} |
                                    {'error', _}.
maybe_build_call_waiting_record(Data, Call) ->
    AccountDb = whapps_call:account_db(Call),
    DocId = get_doc_id(wh_json:get_value(<<"scope">>, Data, <<"device">>), Call),
    case maybe_get_doc(AccountDb, DocId) of
        {'error', _}= _E -> _E;
        {'ok', JObj} ->
            lager:info("changing call waiting settings on document ~s", [wh_doc:id(JObj)]),
            {'ok', #call_waiting{enabled = wh_json:is_true([<<"call_waiting">>, <<"enabled">>], JObj, 'true')
                                 ,jobj = JObj
                                 ,account_db = AccountDb
                                 ,action = wh_json:get_value(<<"action">>, Data, <<"toggle">>)
                                }
            }
    end.

-spec get_doc_id(ne_binary(), whapps_call:call()) -> api_binary().
get_doc_id(<<"user">>, Call) ->
    whapps_call:owner_id(Call);
get_doc_id(<<"device">>, Call) ->
    whapps_call:authorizing_id(Call).

-spec maybe_get_doc(api_binary(), api_binary()) -> wh_jobj_return().
maybe_get_doc(_, 'undefined') ->
    {'error', 'no_device_id'};
maybe_get_doc('undefined', _) ->
    {'error', 'no_account_db'};
maybe_get_doc(AccountDb, Id) ->
    couch_mgr:open_doc(AccountDb, Id).

-spec maybe_execute_action(call_waiting(), whapps_call:call()) -> 'ok' | 'error'.
maybe_execute_action(#call_waiting{action = Action}=CW, Call) ->
    case props:get_value(Action, actions()) of
        'undefined' ->
            lager:info("unsupported call forwaring action ~s", [Action]),
            whapps_call_command:b_prompt(<<"cw-not_available">>, Call);
        ActionFun -> execute_action(ActionFun, CW, Call)
    end.

-spec execute_action(switch_fun(), call_waiting(), whapps_call:call()) -> 'ok' | 'error'.
execute_action(ActionFun, #call_waiting{enabled = Enabled}=CW, Call) ->
    NewEnabled = ActionFun(Enabled),
    _ = play_related_media(NewEnabled, Call),
    maybe_update_doc(NewEnabled, CW).

-spec play_related_media(boolean(), whapps_call:call()) -> any().
play_related_media('true', Call) ->
    lager:debug("call waiting enabled"),
    whapps_call_command:b_prompt(<<"cw-activated">>, Call);
play_related_media('false', Call) ->
    lager:debug("call waiting disabled"),
    whapps_call_command:b_prompt(<<"cw-deactivated">>, Call).

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
    Updated = wh_json:set_value([<<"call_waiting">>, <<"enabled">>], Enabled, JObj),
    case couch_mgr:save_doc(AccountDb, Updated) of
        {'ok', _} ->
            lager:info("call_waiting.enabled set to ~s on ~s ~s"
                       ,[Enabled, wh_doc:type(JObj), wh_doc:id(JObj)]
                      );
        {'error', 'conflict'} ->
            retry_update_doc(Enabled, CW, Retries);
        {'error', _R} ->
            lager:debug("unabled to update call_waiting.enabled on ~s ~s: ~p"
                        ,[wh_doc:type(JObj), wh_doc:id(JObj), _R]
                       ),
            'error'
    end.

-spec retry_update_doc(boolean(), call_waiting(), 0..3) -> 'ok' | 'error'.
retry_update_doc(_Enabled, #call_waiting{jobj = JObj}, Retries) when Retries >= 3 ->
    lager:info("to many attempts to update call_waiting for ~s ~s: ~p"
               ,[wh_doc:type(JObj), wh_doc:id(JObj)]
              ),
    'error';
retry_update_doc(Enabled
                 ,#call_waiting{jobj = JObj
                                ,account_db = AccountDb
                               }=CW
                 ,Retries
                ) ->
    case couch_mgr:open_doc(AccountDb, wh_doc:id(JObj)) of
        {'ok', NewJObj} ->
            maybe_update_doc(Enabled, CW#call_waiting{jobj = NewJObj}, Retries+1);
        {'error', _R} ->
            lager:info("unable to retry call_waiting update for ~s ~s: ~p"
                       ,[wh_doc:type(JObj), wh_doc:id(JObj), _R]
                      ),
            'error'
    end.
