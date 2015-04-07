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

-record(call_waiting, {disabled = 'false' :: boolean()
                       ,jobj = wh_json:new() :: wh_json:object()
                       ,account_db :: api_binary()
                      }).
-type call_waiting() :: #call_waiting{}.

-type switch_fun() :: fun((boolean()) -> boolean()).
-spec actions() -> wh_proplist_kv(ne_binary(), switch_fun()).
actions() ->
    [{<<"toggle">>, fun (Bool) -> not Bool end}
     ,{<<"activate">>, fun (_Bool) -> 'false' end}
     ,{<<"deactivate">>, fun (_Bool) -> 'true' end}
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
            lager:info("unable to determine what document to apply dnd against", []),
            _ = whapps_call_command:b_prompt(<<"cw-not_available">>, Call),
            cf_exe:stop(Call);
        {'ok', #call_waiting{} = CW} ->
            _ = whapps_call_command:answer(Call),
            Action = wh_json:get_value(<<"action">>, Data, <<"toggle">>),
            _ = maybe_execute_action(Action, CW, Call),
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
            lager:info("changing call waiting settings on document ~s", [wh_json:get_value(<<"_id">>, JObj)]),
            {'ok', #call_waiting{disabled = wh_json:is_true(<<"disable_call_waiting">>, JObj, 'false')
                                 ,jobj = JObj
                                 ,account_db = AccountDb
                                }}
    end.

-spec get_doc_id(ne_binary(), wh_json:object()) -> ne_binary().
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
    case couch_mgr:open_doc(AccountDb, Id) of
        {'ok', _} = Ok -> Ok;
        {'error', _R} = E ->
            lager:error("unable to open ~s: ~p", [Id, _R]),
            E
    end.

-spec maybe_execute_action(ne_binary(), call_waiting(), whapps_call:call()) -> any().
maybe_execute_action(Action, #call_waiting{disabled = CWFlag, jobj = JObj, account_db = AccountDB}, Call) ->
    SwitchFun = props:get_value(Action, actions(), fun wh_util:identity/1),
    NewCWFlag = SwitchFun(CWFlag),
    lager:debug("Switch `disable_call_waiting` from ~p to ~p with action ~p", [CWFlag, NewCWFlag, Action]),
    maybe_update_doc(NewCWFlag, JObj, AccountDB),
    play_related_media(NewCWFlag, Call).

-spec play_related_media(boolean(), whapps_call:call()) -> any().
play_related_media('true', Call) ->
    lager:info("Call waiting disabled"),
    whapps_call_command:b_prompt(<<"cw-deactivated">>, Call);
play_related_media('false', Call) ->
    lager:info("Call waiting enabled"),
    whapps_call_command:b_prompt(<<"cw-activated">>, Call).

-spec maybe_update_doc(boolean(), wh_json:object(), ne_binary()) -> wh_jobj_return().
maybe_update_doc(Flag, JObj, AccountDb) ->
    Updated = wh_json:set_value(<<"disable_call_waiting">>, Flag, JObj),
    case couch_mgr:save_doc(AccountDb, Updated) of
        {'ok', _}=Ok ->
            lager:debug("`disable_call_waiting` set to ~s on document ~s"
                        ,[Flag, wh_json:get_value(<<"_id">>, JObj)]),
            Ok;
        {'error', 'conflict'} ->
            case couch_mgr:open_doc(AccountDb, wh_json:get_value(<<"_id">>, JObj)) of
                {'error', _}=E -> E;
                {'ok', NewJObj} -> maybe_update_doc(Flag, NewJObj, AccountDb)
            end;
        {'error', _}=E -> E
    end.
