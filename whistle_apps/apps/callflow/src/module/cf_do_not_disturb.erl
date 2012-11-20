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
            _ = whapps_call_command:b_prompt(<<"dnd-not_available">>, Call),
            cf_exe:stop(Call);
        {ok, #dnd{}=DND} ->
            _ = whapps_call_command:answer(Call),
            Action = wh_json:get_value(<<"action">>, Data),
            maybe_execute_action(Action, DND, Call),
            cf_exe:continue(Call)
    end.            

maybe_build_dnd_record(Data, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case maybe_get_data_id(AccountDb, Data, Call) of
        {error, _}=E -> E;
        {ok, JObj} ->
            {ok, #dnd{enabled=wh_json:is_true([<<"do_not_distrub">>, <<"enabled">>], JObj)
                      ,jobj=JObj
                      ,account_db=AccountDb
                     }}
    end.

maybe_get_data_id(AccountDb, Data, Call) ->
    Id = wh_json:get_value(<<"id">>, Data),
    case maybe_get_doc(AccountDb, Id) of
        {error, _} -> maybe_get_owner(AccountDb, Data, Call);
        {ok, _}=Ok -> Ok
    end.

maybe_get_owner(AccountDb, Data, Call) ->
    OwnerId = whapps_call:owner_id(Call),
    case maybe_get_doc(AccountDb, OwnerId) of
        {error, _} -> maybe_get_authorizing_device(AccountDb, Data, Call);
        {ok, _}=Ok -> Ok
    end.

maybe_get_authorizing_device(AccountDb, _, Call) ->
    AuthorizingId = whapps_call:authorizing_id(Call),
    maybe_get_doc(AccountDb, AuthorizingId).

maybe_get_doc(_, undefined) ->
    {error, no_device_id};
maybe_get_doc(undefined, _) ->
    {error, no_account_db};
maybe_get_doc(AccountDb, Id) ->
    case couch_mgr:open_doc(AccountDb, Id) of
        {ok, JObj}=Ok ->
            case wh_json:is_true(<<"pvt_type">>, JObj) of
                <<"device">> -> Ok;
                <<"user">> -> Ok;
                false -> {error, not_found}
            end;
         {error, _R}=E ->
            lager:debug("unable to open ~s: ~p", [Id, _R]),
            E
    end.

maybe_execute_action(<<"activate">>, #dnd{enabled=true}, Call) ->
    whapps_call_command:b_prompt(<<"dnd-activated">>, Call);
maybe_execute_action(<<"activate">>, #dnd{enabled=false}=DND, Call) ->
    activate_dnd(DND, Call);
maybe_execute_action(<<"deactivate">>, #dnd{enabled=false}, Call) ->
    whapps_call_command:b_prompt(<<"dnd-deactivated">>, Call);
maybe_execute_action(<<"deactivate">>,  #dnd{enabled=true}=DND, Call) ->
    deactivate_dnd(DND, Call);
maybe_execute_action(<<"toggle">>, #dnd{enabled=false}=DND, Call) ->
    maybe_execute_action(<<"activate">>, DND, Call);
maybe_execute_action(<<"toggle">>,  #dnd{enabled=true}=DND, Call) ->
    maybe_execute_action(<<"deactivate">>, DND, Call);
maybe_execute_action(_, _, Call) ->
    whapps_call_command:b_prompt(<<"dnd-not_available">>, Call).

activate_dnd(#dnd{jobj=JObj, account_db=AccountDb}, Call) ->
    case maybe_update_doc(true, JObj, AccountDb) of
        {error, _} -> ok;
        {ok, _} ->
            whapps_call_command:b_prompt(<<"dnd-activated">>, Call)
    end.            

deactivate_dnd(#dnd{jobj=JObj, account_db=AccountDb}, Call) ->
    case maybe_update_doc(false, JObj, AccountDb) of
        {error, _} -> ok;
        {ok, _} ->
            whapps_call_command:b_prompt(<<"dnd-deactivated">>, Call)
    end.

maybe_update_doc(Enabled, JObj, AccountDb) ->
    Updated = wh_json:set_value(<<"do_not_disturb">>, Enabled, JObj),
    case couch_mgr:save_doc(AccountDb, Updated) of
        {ok, _}=Ok -> Ok;
        {error, conflict} ->
            Id = wh_json:get_value(<<"_id">>, JObj),
            case couch_mgr:open_doc(AccountDb, Id) of
                {error, _}=E -> E;
                {ok, NewJObj} -> 
                    maybe_update_doc(Enabled, NewJObj, AccountDb)
            end;
        {error, _}=E -> E         
    end.
