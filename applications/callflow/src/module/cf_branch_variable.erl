%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Try to branch based on the value of a variable.
%%%
%%% <h4>Data options:</h4>
%%% <dl>
%%%   <dt>`variable'</dt>
%%%   <dd>The name of Callflow child name to branch into</dd>
%%%
%%%   <dt>`scope'</dt>
%%%   <dd><strong>Optional: </strong>The scope which is `variable' is defined.
%%%   Possible scopes are `custom_channel_vars', `device', `user', `account'
%%%   and `merged'.
%%%   </dd>
%%% </dl>
%%%
%%%
%%% @author Sponsored by GTNetwork LLC, Implemented by SIPLABS LLC
%%% @author SIPLABS LLC (Maksim Krzhemenevskiy)
%%% @author Hesaam Farhang
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_branch_variable).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

-type variable_key() :: kz_term:api_ne_binary() | kz_term:api_ne_binaries().

%%------------------------------------------------------------------------------
%% @doc Entry point for this module.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Scope = kz_json:get_ne_binary_value(<<"scope">>, Data, <<"custom_channel_vars">>),
    Variable = normalize_variable(kz_json:get_ne_value(<<"variable">>, Data)),
    lager:info("looking for variable '~p' value in scope '~s' to find next callflow branch", [Variable, Scope]),
    ChildName = find_child_in_scope(Scope, Variable, Call),
    maybe_branch_to_named_child(ChildName, Call).

-spec maybe_branch_to_named_child(variable_key(), kapps_call:call()) -> 'ok'.
maybe_branch_to_named_child('undefined', Call) ->
    lager:info("trying '_'"),
    cf_exe:continue(Call);
maybe_branch_to_named_child(ChildName, Call) ->
    lager:info("trying '~s'", [ChildName]),
    cf_exe:continue(ChildName, Call).

%%------------------------------------------------------------------------------
%% @doc Try to find callflow branch name out of variable's value in the Scope.
%% If scope sets to other values than custom_channel_vars, account,
%% user and device, it search in merged attributes in endpoint.
%% @end
%%------------------------------------------------------------------------------
-spec find_child_in_scope(kz_term:ne_binary(), variable_key(), kapps_call:call()) -> kz_term:api_binary().
find_child_in_scope(_Scope, 'undefined', _Call) ->
    lager:warning("no variable is specified"),
    'undefined';
find_child_in_scope(<<"custom_channel_vars">>, Variable, Call) ->
    ChildName = kz_json:get_ne_value(Variable, kz_json:normalize(kapps_call:custom_channel_vars(Call))),
    find_child_in_branch(ChildName, Call);
find_child_in_scope(<<"account">>, Variable, Call) ->
    find_child_in_doc(kapps_call:account_id(Call), Variable, Call);
find_child_in_scope(Scope, Variable, Call) when Scope =:= <<"user">>;
                                                Scope =:= <<"device">> ->
    case kapps_call:authorizing_type(Call) of
        <<"device">> when Scope =:= <<"user">> ->
            find_child_in_doc(device_owner(Call), Variable, Call);
        <<"device">> when Scope =:= <<"device">> ->
            find_child_in_doc(kapps_call:authorizing_id(Call), Variable, Call);
        <<"mobile">> when Scope =:= <<"device">> ->
            find_child_in_doc(kapps_call:authorizing_id(Call), Variable, Call);
        _AuthType ->
            lager:debug("unsupported authorizing type: ~s", [_AuthType]),
            'undefined'
    end;
find_child_in_scope(<<"merged">>, Variable, Call) ->
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    CCVsChild = kz_json:get_ne_value(Variable, kz_json:normalize(kapps_call:custom_channel_vars(Call))),
    case kz_endpoint:get(Call) of
        {'ok', JObj} ->
            EndPointChildName = kz_json:get_ne_value(Variable, JObj),
            case find_child_in_branch(EndPointChildName, Call, Keys) of
                'undefined' ->
                    find_child_in_branch(CCVsChild, Call, Keys);
                ChildName -> ChildName
            end;
        _Else ->
            lager:debug("failed to lookup endpoint"),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Open the document DocId to find the value of the variable which is the
%% name of the callflow branch.
%% @end
%%------------------------------------------------------------------------------
-spec find_child_in_doc(kz_term:api_binary(), kz_term:ne_binary(), kapps_call:call()) -> kz_term:api_binary().
find_child_in_doc('undefined', _Variable, _Call) ->
    lager:debug("could not find document for current scope"),
    'undefined';
find_child_in_doc(DocId, Variable, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DocId) of
        {'ok', JObj} ->
            find_child_in_branch(kz_json:get_ne_value(Variable, JObj), Call);
        _Else ->
            lager:debug("failed to open device doc ~s in account ~s", [DocId, kapps_call:account_id(Call)]),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Look into the defined children in the callflow to see if the child name
%% that was found is there, if yes we found the child, otherwise
%% fall back to the default children '_'.
%% @end
%%------------------------------------------------------------------------------
-spec find_child_in_branch(any(), kapps_call:call()) -> kz_term:api_binary().
find_child_in_branch(ChildName, Call) ->
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    find_child_in_branch(ChildName, Call, Keys).

-spec find_child_in_branch(any(), kapps_call:call(), kz_json:paths()) -> kz_term:api_binary().
find_child_in_branch('undefined', _Call, _Keys) -> 'undefined';
find_child_in_branch(?NE_BINARY = ChildName, _Call, Keys) ->
    case lists:member(ChildName, Keys) of
        'true' -> ChildName;
        'false' -> 'undefined'
    end;
find_child_in_branch(ChildName, Call, Keys) ->
    try kz_term:to_binary(ChildName) of
        Bin ->
            find_child_in_branch(Bin, Call, Keys)
    catch
        _:_ ->
            lager:info("failed to convert none binary value ~p to binary", [ChildName]),
            'undefined'
    end.

%% Utility Functions

-spec device_owner(kapps_call:call()) -> kz_term:ne_binary().
device_owner(Call) ->
    DeviceId = kapps_call:authorizing_id(Call),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DeviceId) of
        {'ok', JObj} ->
            maybe_hotdesked_device(JObj);

        _Else ->
            lager:debug("failed to open device doc ~s in account ~s", [DeviceId, kapps_call:account_id(Call)]),
            'undefined'
    end.

-spec maybe_hotdesked_device(kz_json:object()) -> kz_term:ne_binary().
maybe_hotdesked_device(Doc) ->
    case kzd_devices:is_hotdesked(Doc) of
        'true' ->
            [UseId|_] = kzd_devices:hotdesk_ids(Doc),
            lager:info("using hotdesked id ~p", [UseId]),
            UseId;

        'false' ->
            Owner = kzd_devices:owner_id(Doc),
            lager:info("using owner id ~p", [Owner]),
            Owner
    end.

%%------------------------------------------------------------------------------
%% @doc Normalize variable. Variable is a json path, so we should accept
%% binary, or a path and ignore others,
%% So if ones wants to look into a deep json object, path `[<<"v1">>, <<"v2">>]'
%% can be used to get the value.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_variable(variable_key() | kz_json:object()) -> variable_key().
normalize_variable('undefined') ->
    'undefined';
normalize_variable(?NE_BINARY = Variable) ->
    kz_json:normalize_key(Variable);
normalize_variable(Variable) when is_list(Variable) ->
    lists:reverse([kz_json:normalize_key(V)
                   || V <- Variable,
                      not kz_term:is_empty(V)
                  ]
                 );
normalize_variable(_JObj) ->
    lager:debug("unsupported variable name"),
    'undefined'.
