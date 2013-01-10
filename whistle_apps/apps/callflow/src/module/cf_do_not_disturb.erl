%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_do_not_disturb).

-include("../callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".do_not_disturb">>).

-record(dnd, {enabled = false
              ,jobj = wh_json:json_object()
              ,account_db = undefined
             }).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case maybe_build_dnd_record(Data, Call) of
        {error, _} ->
            lager:info("unable to determine what document to apply dnd against", []),
            _ = whapps_call_command:b_prompt(<<"dnd-not_available">>, Call),
            cf_exe:stop(Call);
        {ok, #dnd{}=DND} ->
            _ = whapps_call_command:answer(Call),
            Action = wh_json:get_value(<<"action">>, Data),
            _ = maybe_execute_action(Action, DND, Call),
            cf_exe:continue(Call)
    end.            

-spec maybe_build_dnd_record/2 :: (wh_json:json_object(), whapps_call:call()) -> {'ok', #dnd{}} | {'error', _}.
maybe_build_dnd_record(Data, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case maybe_get_data_id(AccountDb, Data, Call) of
        {error, _}=E -> E;
        {ok, JObj} ->
            lager:info("changing dnd settings on document ~s", [wh_json:get_value(<<"_id">>, JObj)]),
            {ok, #dnd{enabled=wh_json:is_true([<<"do_not_disturb">>, <<"enabled">>], JObj)
                      ,jobj=JObj
                      ,account_db=AccountDb
                     }}
    end.

-spec maybe_get_data_id/3 :: (ne_binary(), wh_json:json_object(), whapps_call:call()) -> wh_jobj_return().
maybe_get_data_id(AccountDb, Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case maybe_get_doc(AccountDb, Id) of
        {error, _} -> 
            lager:info("dnd feature callflow does not specify a document", []),
            maybe_get_owner(AccountDb, Data, Call);
        {ok, _}=Ok -> Ok
    end.

-spec maybe_get_owner/3 :: (ne_binary(), wh_json:json_object(), whapps_call:call()) -> wh_jobj_return().
maybe_get_owner(AccountDb, Data, Call) ->
    OwnerId = whapps_call:owner_id(Call),
    case maybe_get_doc(AccountDb, OwnerId) of
        {error, _} -> 
            lager:info("dnd feature could not find the owner document", []),
            maybe_get_authorizing_device(AccountDb, Data, Call);
        {ok, _}=Ok -> Ok
    end.

-spec maybe_get_authorizing_device/3 :: (ne_binary(), wh_json:json_object(), whapps_call:call()) -> wh_jobj_return().
maybe_get_authorizing_device(AccountDb, _, Call) ->
    AuthorizingId = whapps_call:authorizing_id(Call),
    maybe_get_doc(AccountDb, AuthorizingId).

-spec maybe_get_doc/2 :: (api_binary(), api_binary()) -> wh_jobj_return().
maybe_get_doc(_, undefined) ->
    {error, no_device_id};
maybe_get_doc(undefined, _) ->
    {error, no_account_db};
maybe_get_doc(AccountDb, Id) ->
    case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj}=Ok ->
            case wh_json:get_value(<<"pvt_type">>, JObj) of
                <<"device">> -> Ok;
                <<"user">> -> Ok;
                _Else ->
                    lager:info("dnd can not be applied against a doc of type ~s", [_Else]),
                    {error, not_found}
            end;
         {error, _R}=E ->
            lager:info("unable to open ~s: ~p", [Id, _R]),
            E
    end.

-spec maybe_execute_action/3 :: (ne_binary(), #dnd{}, whapps_call:call()) -> any().
maybe_execute_action(<<"activate">>, #dnd{enabled=true}, Call) -> 
    lager:info("dnd is already enabled on this document", []),
    whapps_call_command:b_prompt(<<"dnd-activated">>, Call);
maybe_execute_action(<<"activate">>, #dnd{enabled=false}=DND, Call) ->
    activate_dnd(DND, Call);
maybe_execute_action(<<"deactivate">>, #dnd{enabled=false}, Call) ->
    lager:info("dnd is already disabled on this document", []),
    whapps_call_command:b_prompt(<<"dnd-deactivated">>, Call);
maybe_execute_action(<<"deactivate">>,  #dnd{enabled=true}=DND, Call) ->
    deactivate_dnd(DND, Call);
maybe_execute_action(<<"toggle">>, #dnd{enabled=false}=DND, Call) ->
    maybe_execute_action(<<"activate">>, DND, Call);
maybe_execute_action(<<"toggle">>,  #dnd{enabled=true}=DND, Call) ->
    maybe_execute_action(<<"deactivate">>, DND, Call);
maybe_execute_action(_Action, _, Call) ->
    lager:info("dnd action ~s is invalid", [_Action]),
    whapps_call_command:b_prompt(<<"dnd-not_available">>, Call).

-spec activate_dnd/2 :: (#dnd{}, whapps_call:call()) -> any().
activate_dnd(#dnd{jobj=JObj, account_db=AccountDb}, Call) ->
    case maybe_update_doc(true, JObj, AccountDb) of
        {error, _} -> ok;
        {ok, _} ->
            whapps_call_command:b_prompt(<<"dnd-activated">>, Call)
    end.            

-spec deactivate_dnd/2 :: (#dnd{}, whapps_call:call()) -> any().
deactivate_dnd(#dnd{jobj=JObj, account_db=AccountDb}, Call) ->
    case maybe_update_doc(false, JObj, AccountDb) of
        {error, _} -> ok;
        {ok, _} ->
            whapps_call_command:b_prompt(<<"dnd-deactivated">>, Call)
    end.

-spec maybe_update_doc/3 :: (boolean(), wh_json:json_object(), ne_binary()) -> wh_jobj_return().
maybe_update_doc(Enabled, JObj, AccountDb) ->
    Updated = wh_json:set_value([<<"do_not_disturb">>, <<"enabled">>], Enabled, JObj),
    case couch_mgr:save_doc(AccountDb, Updated) of
        {ok, _}=Ok -> 
            DocId = wh_json:get_value(<<"_id">>, JObj),
            lager:info("dnd enabled set to ~s on document ~s", [Enabled, DocId]),
            Ok;
        {error, conflict} ->
            Id = wh_json:get_value(<<"_id">>, JObj),
            case couch_mgr:open_doc(AccountDb, Id) of
                {error, _}=E -> E;
                {ok, NewJObj} -> 
                    maybe_update_doc(Enabled, NewJObj, AccountDb)
            end;
        {error, _}=E -> E         
    end.
