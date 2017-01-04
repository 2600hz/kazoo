%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2017, 2600Hz INC
%%% @doc
%%%
%%% Try to branch based on the value of a variable.
%%%
%%% "data":{
%%%   "variable":{{var_name}}
%%%   //optional
%%%   "scope": "[custome_channel_vars|device|user|account|_]",
%%% }
%%%
%%% @end
%%% @contributors
%%%   KAZOO-3596: Sponsored by GTNetwork LLC, implemented by SIPLABS LLC
%%%   KAZOO-4601: SIPLABS LLC (Maksim Krzhemenevskiy)
%%%   Hesaam Farhang
%%%-------------------------------------------------------------------
-module(cf_branch_variable).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Scope = kz_json:get_ne_binary_value(<<"scope">>, Data, <<"custom_channel_vars">>),
    Variable = kz_json:normalize_key(kz_json:get_ne_binary_value(<<"variable">>, Data, <<>>)),
    lager:info("looking for variable ~s value in scope ~s to find next callflow branch", [Scope, Variable]),
    ChildName = find_child_in_scope(Scope, Variable, Call),
    maybe_branch_to_named_child(ChildName, Call).

-spec maybe_branch_to_named_child(api_binary(), kapps_call:call()) -> kapps_call:call().
maybe_branch_to_named_child('undefined', Call) ->
    lager:info("trying '_'"),
    cf_exe:continue(Call);
maybe_branch_to_named_child(ChildName, Call) ->
    lager:info("trying '~s'", [ChildName]),
    cf_exe:continue(ChildName, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Try to find callflow branch name out of variable's value in the Scope.
%% If scope sets to other values than custome_channel_vars, account,
%% user and device, it search in merged attributes in endpoint.
%% @end
%%--------------------------------------------------------------------
-spec find_child_in_scope(ne_binary(), binary(), kapps_call:call()) -> api_binary().
find_child_in_scope(_Scope, <<>>, _Call) ->
    lager:warning("no variable is specified"),
    'undefined';
find_child_in_scope(<<"custom_channel_vars">>, Variable, Call) ->
    ChildName = kz_json:get_ne_binary_value(Variable, kz_json:normalize(kapps_call:custom_channel_vars(Call))),
    find_child_in_branch(ChildName, Call);
find_child_in_scope(<<"account">>, Variable, Call) ->
    find_child_in_doc(kapps_call:account_id(Call), Variable, Call);
find_child_in_scope(Scope, Variable, Call) when Scope =:= <<"user">>;
                                                Scope =:= <<"device">> ->
    case kapps_call:authorizing_type(Call) of
        <<"device">> when Scope =:= <<"user">>->
            find_child_in_doc(device_owner(Call), Variable, Call);
        <<"device">> when Scope =:= <<"device">>->
            find_child_in_doc(kapps_call:authorizing_id(Call), Variable, Call);
        _AuthType ->
            lager:debug("unsupported authorizing type: ~s", [_AuthType]),
            'undefined'
    end;
find_child_in_scope(_Scope, Variable, Call) ->
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    CCVsChild = kz_json:get_ne_binary_value(Variable, kz_json:normalize(kapps_call:custom_channel_vars(Call))),
    case kz_endpoint:get(Call) of
        {'ok', JObj} ->
            EndPointChildName = kz_json:get_value(Variable, JObj),
            case lists:member(EndPointChildName, Keys) of
                'true' -> EndPointChildName;
                'false' ->
                    case lists:member(CCVsChild, Keys) of
                        'true' -> CCVsChild;
                        'false' -> 'undefined'
                    end
            end;
        _Else ->
            lager:debug("failed to lookup endpoint"),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Open the document DocId to find the value of the variable which is the
%% name of the callflow branch.
%% @end
%%--------------------------------------------------------------------
-spec find_child_in_doc(api_binary(), ne_binary(), kapps_call:call()) -> api_binary().
find_child_in_doc('undefined', _Variable, _Call) ->
    lager:debug("could not find document for current scope"),
    'undefined';
find_child_in_doc(DocId, Variable, Call) ->
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DocId) of
        {'ok', JObj} ->
            find_child_in_branch(kz_json:get_ne_binary_value(Variable, JObj), Call);
        _Else ->
            lager:debug("failed to open device doc ~s in account ~s", [DocId, kapps_call:account_id(Call)]),
            'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Look into the defined children in the callflow to see if the child name
%% that was found is there, if yes we found the child, otherwise
%% fall back to the default children '_'.
%% @end
%%--------------------------------------------------------------------
-spec find_child_in_branch(api_binary(), kapps_call:call()) -> api_binary().
find_child_in_branch(ChildName, Call) ->
    {'branch_keys', Keys} = cf_exe:get_branch_keys(Call),
    case lists:member(ChildName, Keys) of
        'true' -> ChildName;
        'false' -> 'undefined'
    end.

-spec device_owner(kapps_call:call()) -> ne_binary().
device_owner(Call) ->
    DeviceId = kapps_call:authorizing_id(Call),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), DeviceId) of
        {'ok', JObj} -> kz_device:owner_id(JObj);
        _Else ->
            lager:debug("failed to open device doc ~s in account ~s", [DeviceId, kapps_call:account_id(Call)]),
            'undefined'
    end.
