%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc Module for extending schema validation
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema_extensions).

-export([extra_validator/3]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(INVALID_STORAGE_ATTACHMENT_REFERENCE(R), <<"invalid reference '", R/binary, "' to attachments">>).
-define(INVALID_STORAGE_CONNECTION_REFERENCE(R), <<"invalid reference '", R/binary, "' to connections">>).

-spec extra_validator(jesse:json_term(), jesse_state:state(), kz_json_schema:extra_validator_options()) -> jesse_state:state().
extra_validator(Value, State, Options) ->
    Routines = [fun stability_level/3
               ,fun extended_validation/3
               ,fun extended_regexp/3
               ],
    lists:foldl(fun(Fun, AccState) -> Fun(Value, AccState, Options) end, State, Routines).

-spec extended_regexp(jesse:json_term(), jesse_state:state(), kz_json_schema:extra_validator_options()) -> jesse_state:state().
extended_regexp(Value, State, _Options) ->
    Schema = jesse_state:get_current_schema(State),
    case kz_json:is_true(<<"kazoo-regexp">>, Schema, 'false') of
        'true' -> regexp_validation(Value, State);
        'false' -> State
    end.

-spec regexp_validation(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
regexp_validation(Value, State) ->
    case re:compile(Value) of
        {'error', _Reason} ->
            ErrMsg = <<"invalid regular expression: '", Value/binary,"'">>,
            jesse_error:handle_data_invalid({'external_error', ErrMsg}, Value, State);
        _ ->
            State
    end.

-spec extended_validation(jesse:json_term(), jesse_state:state(), kz_json_schema:extra_validator_options()) -> jesse_state:state().
extended_validation(Value, State, _Options) ->
    Schema = jesse_state:get_current_schema(State),
    case kz_json:is_true(<<"kazoo-validation">>, Schema, 'false') of
        'true' -> extra_validation(Value, State);
        'false' -> State
    end.

-spec extra_validation(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
extra_validation(Value, State) ->
    SchemaId = jesse_state:get_current_schema_id(State),
    Path = lists:reverse(jesse_state:get_current_path(State)),

    ElementId = kz_term:to_binary(lists:last(Path)),
    Keys = [SchemaId, ElementId],
    Key = kz_binary:join(lists:filter(fun kz_term:is_not_empty/1, Keys), <<".">>),
    extra_validation(Key, Value, State).

extra_validation(<<"metaflow.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    Module = jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, 'undefined'),
    lager:debug("validating metaflow action '~s' with data ~p", [Module, Value]),
    validate_module_data(<<"metaflows.", Module/binary>>, Value, State);

extra_validation(<<"metaflow.module">>, Value, State) ->
    lager:debug("validating metaflow action '~s'", [Value]),
    Schema = <<"metaflows.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid({'external_error', <<"unable to find metaflow schema for module ", Value/binary>>}, Value, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"callflows.action.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    case jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, 'undefined') of
        'undefined' -> State;
        Module -> validate_module_data(<<"callflows.", Module/binary>>, Value, State)
    end;
extra_validation(<<"callflows.action.module">>, Value, State) ->
    lager:debug("validating callflow action '~s'", [Value]),
    Schema = <<"callflows.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid({'external_error', <<"unable to find callflow schema for module ", Value/binary>>}, Value, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"storage.plan.database.document.connection">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"connections">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid({'external_error' ,?INVALID_STORAGE_CONNECTION_REFERENCE(Value)}
                                                  ,Value
                                                  ,State
                                                  )
    end;
extra_validation(<<"storage.plan.database.attachment.handler">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"attachments">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid({'external_error', ?INVALID_STORAGE_ATTACHMENT_REFERENCE(Value)}
                                                  ,Value
                                                  ,State
                                                  )
    end;
extra_validation(<<"storage.attachment.google_drive.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.google_storage.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.onedrive.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(<<"storage.attachment.dropbox.oauth_doc_id">>, Value, State) ->
    validate_attachment_oauth_doc_id(Value, State);
extra_validation(_Key, _Value, State) ->
    lager:debug("extra validation of ~s not handled for value ~p", [_Key, _Value]),
    State.

validate_module_data(Schema, Value, State) ->
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema ->
                     SchemaObj = jesse_state:get_current_schema(State1),
                     jesse_schema_validator:validate_with_state(SchemaObj, Value, State1);
                 _OtherSchema -> State1
             end,
    jesse_state:undo_resolve_ref(State2, State).

validate_attachment_oauth_doc_id(Value, State) ->
    lager:debug("validating oauth_doc_id: ~s", [Value]),
    case kz_datamgr:open_doc(<<"system_auth">>, Value) of
        {'ok', _Obj} ->
            State;
        {'error', 'empty_doc_id'} ->
            ErrorMsg = <<"empty oauth_doc_id">>,
            jesse_error:handle_data_invalid({'external_error', ErrorMsg}, Value, State);
        {'error', 'not_found'} ->
            ErrorMsg = <<"Invalid oauth_doc_id: ", Value/binary>>,
            lager:debug("~s", [ErrorMsg]),
            jesse_error:handle_data_invalid({'external_error', ErrorMsg}, Value, State);
        {'error', _Err} ->
            ErrorMsg = <<"Error validating oauth_doc_id: ", Value/binary>>,
            lager:debug("~s : ~p", [ErrorMsg, _Err]),
            jesse_error:handle_data_invalid({'external_error', ErrorMsg}, Value, State)
    end.

-spec stability_level(jesse:json_term(), jesse_state:state(), kz_json_schema:extra_validator_options()) -> jesse_state:state().
stability_level(Value, State, Options) ->
    Schema = jesse_state:get_current_schema(State),
    SystemSL = props:get_ne_binary_value('stability_level', Options),
    ParamSL = kz_json:get_value(<<"stability_level">>, Schema),
    maybe_check_param_stability_level(SystemSL, ParamSL, Value, State).

maybe_check_param_stability_level('undefined', _ParamSL, _Value, State) ->
    State; %% SystemSL is undefined, skip checking
maybe_check_param_stability_level(_SystemSL, 'undefined', _Value, State) ->
    State; %% ParamSL is undefined, skip checking
maybe_check_param_stability_level(SystemSL, ParamSL, Value, State) ->
    SystemSLInt = stability_level_to_int(SystemSL),
    ParamSLInt = stability_level_to_int(ParamSL),
    case check_param_stability_level(SystemSLInt, ParamSLInt) of
        'valid' ->
            State;
        'invalid' ->
            ErrorMsg = <<"Disallowed parameter, it has lower stability level ("
                        ,ParamSL/binary, ") than system's stability level ("
                        ,SystemSL/binary, ")"
                       >>,
            lager:debug("~s", [ErrorMsg]),
            jesse_error:handle_data_invalid({'external_error', ErrorMsg}, Value, State)
    end.

check_param_stability_level(SystemSLInt, ParamSLInt) when ParamSLInt < SystemSLInt ->
    'invalid';
check_param_stability_level(_SystemSLInt, _ParamSLInt) ->
    'valid'.

-spec stability_level_to_int(kz_term:ne_binary()) -> pos_integer().
stability_level_to_int(<<"stable">>) -> 3;
stability_level_to_int(<<"beta">>) -> 2;
stability_level_to_int(<<"alpha">>) -> 1.
