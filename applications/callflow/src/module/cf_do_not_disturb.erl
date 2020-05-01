%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_do_not_disturb).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".do_not_disturb">>).

-record(dnd, {enabled = 'false' :: boolean()
             ,jobj = kz_json:new() :: kz_json:object()
             ,account_db :: kz_term:api_binary()
             }).
-type dnd() :: #dnd{}.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successful.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    case maybe_build_dnd_record(Data, Call) of
        {'error', _} ->
            lager:info("unable to determine what document to apply dnd against", []),
            _ = kapps_call_command:b_prompt(<<"dnd-not_available">>, Call),
            cf_exe:stop(Call);
        {'ok', #dnd{}=DND} ->
            _ = kapps_call_command:answer(Call),
            Action = kz_json:get_ne_binary_value(<<"action">>, Data),
            _ = maybe_execute_action(Action, DND, Call),
            cf_exe:continue(Call)
    end.

-spec maybe_build_dnd_record(kz_json:object(), kapps_call:call()) ->
          {'ok', dnd()} |
          {'error', any()}.
maybe_build_dnd_record(Data, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case maybe_get_data_id(AccountDb, Data, Call) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            lager:info("changing dnd settings on document ~s", [kz_doc:id(JObj)]),
            {'ok', #dnd{enabled=kz_json:is_true([<<"do_not_disturb">>, <<"enabled">>], JObj)
                       ,jobj=JObj
                       ,account_db=AccountDb
                       }}
    end.

-spec maybe_get_data_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:jobj_return().
maybe_get_data_id(AccountDb, Data, Call) ->
    Id = kz_json:get_ne_binary_value(<<"id">>, Data),
    case maybe_get_doc(AccountDb, Id) of
        {'error', _} ->
            lager:info("dnd feature callflow does not specify a document"),
            maybe_get_owner(AccountDb, Call);
        {'ok', _}=Ok -> Ok
    end.

-spec maybe_get_owner(kz_term:ne_binary(), kapps_call:call()) -> kz_term:jobj_return().
maybe_get_owner(AccountDb, Call) ->
    OwnerId = kapps_call:owner_id(Call),
    case maybe_get_doc(AccountDb, OwnerId) of
        {'error', _} ->
            lager:info("dnd feature could not find the owner document"),
            maybe_get_authorizing_device(AccountDb, Call);
        {'ok', _}=Ok -> Ok
    end.

-spec maybe_get_authorizing_device(kz_term:ne_binary(), kapps_call:call()) -> kz_term:jobj_return().
maybe_get_authorizing_device(AccountDb, Call) ->
    AuthorizingId = kapps_call:authorizing_id(Call),
    maybe_get_doc(AccountDb, AuthorizingId).

-spec maybe_get_doc(kz_term:api_binary(), kz_term:api_binary()) -> kz_term:jobj_return().
maybe_get_doc(_, 'undefined') ->
    {'error', 'no_device_id'};
maybe_get_doc('undefined', _) ->
    {'error', 'no_account_db'};
maybe_get_doc(AccountDb, Id) ->
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', JObj}=Ok ->
            case kz_doc:type(JObj) of
                <<"device">> -> Ok;
                <<"user">> -> Ok;
                _Else ->
                    lager:info("dnd can not be applied against a doc of type ~s", [_Else]),
                    {'error', 'not_found'}
            end;
        {'error', _R}=E ->
            lager:info("unable to open ~s: ~p", [Id, _R]),
            E
    end.

-spec maybe_execute_action(kz_term:ne_binary(), dnd(), kapps_call:call()) -> any().
maybe_execute_action(<<"activate">>, #dnd{enabled='true'}, Call) ->
    lager:info("dnd is already enabled on this document", []),
    kapps_call_command:b_prompt(<<"dnd-activated">>, Call);
maybe_execute_action(<<"activate">>, #dnd{enabled='false'}=DND, Call) ->
    activate_dnd(DND, Call);
maybe_execute_action(<<"deactivate">>, #dnd{enabled='false'}, Call) ->
    lager:info("dnd is already disabled on this document", []),
    kapps_call_command:b_prompt(<<"dnd-deactivated">>, Call);
maybe_execute_action(<<"deactivate">>,  #dnd{enabled='true'}=DND, Call) ->
    deactivate_dnd(DND, Call);
maybe_execute_action(<<"toggle">>, #dnd{enabled='false'}=DND, Call) ->
    maybe_execute_action(<<"activate">>, DND, Call);
maybe_execute_action(<<"toggle">>,  #dnd{enabled='true'}=DND, Call) ->
    maybe_execute_action(<<"deactivate">>, DND, Call);
maybe_execute_action(_Action, _, Call) ->
    lager:info("dnd action ~s is invalid", [_Action]),
    kapps_call_command:b_prompt(<<"dnd-not_available">>, Call).

-spec activate_dnd(dnd(), kapps_call:call()) -> any().
activate_dnd(#dnd{jobj=JObj
                 ,account_db=AccountDb
                 }, Call) ->
    case maybe_update_doc('true', JObj, AccountDb) of
        {'error', _} -> 'ok';
        {'ok', _} -> kapps_call_command:b_prompt(<<"dnd-activated">>, Call)
    end.

-spec deactivate_dnd(dnd(), kapps_call:call()) -> any().
deactivate_dnd(#dnd{jobj=JObj
                   ,account_db=AccountDb
                   }, Call) ->
    case maybe_update_doc('false', JObj, AccountDb) of
        {'error', _} -> 'ok';
        {'ok', _} -> kapps_call_command:b_prompt(<<"dnd-deactivated">>, Call)
    end.

-spec maybe_update_doc(boolean(), kz_json:object(), kz_term:ne_binary()) -> kz_term:jobj_return().
maybe_update_doc(Enabled, JObj, AccountDb) ->
    Updated = kz_json:set_value([<<"do_not_disturb">>, <<"enabled">>], Enabled, JObj),
    case kz_datamgr:save_doc(AccountDb, Updated) of
        {'ok', _}=Ok ->
            lager:info("dnd enabled set to ~s on document ~s", [Enabled, kz_doc:id(JObj)]),
            Ok;
        {'error', 'conflict'} ->
            case kz_datamgr:open_doc(AccountDb, kz_doc:id(JObj)) of
                {'error', _}=E -> E;
                {'ok', NewJObj} -> maybe_update_doc(Enabled, NewJObj, AccountDb)
            end;
        {'error', _}=E -> E
    end.
