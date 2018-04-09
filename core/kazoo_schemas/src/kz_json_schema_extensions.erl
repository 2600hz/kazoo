%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2018, 2600Hz
%%% @doc Module for extending schema validation
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_json_schema_extensions).

-export([extra_validator/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(INVALID_STORAGE_ATTACHMENT_REFERENCE(R), <<"invalid reference '", R/binary, "' to attachments">>).
-define(INVALID_STORAGE_CONNECTION_REFERENCE(R), <<"invalid reference '", R/binary, "' to connections">>).

-spec extra_validator(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
extra_validator(Value, State) ->
    Schema = jesse_state:get_current_schema(State),
    SystemSL = kapps_config:get_binary(<<"crossbar">>, <<"stability_level">>),
    ParamSL = kz_json:get_value(<<"stability_level">>, Schema),
    State1 = maybe_check_param_stability_level(SystemSL, ParamSL, State),
    case kz_json:is_true(<<"kazoo-validation">>, Schema, 'false') of
        'true' -> extra_validation(Value, State1);
        'false' -> State1
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
                 _OtherSchema -> jesse_error:handle_data_invalid('external_error', <<"unable to find metaflow schema for module ", Value/binary>>, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"callflows.action.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    case jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, undefined) of
        'undefined' -> State;
        Module -> validate_module_data(<<"callflows.", Module/binary>>, Value, State)
    end;
extra_validation(<<"callflows.action.module">>, Value, State) ->
    lager:debug("validating callflow action '~s'", [Value]),
    Schema = <<"callflows.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid(external_error, <<"unable to find callflow schema for module ", Value/binary>>, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"storage.plan.database.document.connection">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"connections">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid('external_error'
                                                  ,?INVALID_STORAGE_CONNECTION_REFERENCE(Value)
                                                  ,State
                                                  )
    end;
extra_validation(<<"storage.plan.database.attachment.handler">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    Keys = kz_json:get_keys(<<"attachments">>, JObj),
    case lists:member(Value, Keys) of
        'true' -> State;
        'false' -> jesse_error:handle_data_invalid('external_error'
                                                  ,?INVALID_STORAGE_ATTACHMENT_REFERENCE(Value)
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
    lager:debug("Validating oauth_doc_id: ~s", [Value]),
    case kz_datamgr:open_doc(<<"system_auth">>, Value) of
        {ok, _Obj} ->
            State;
        {error, not_found} ->
            ErrorMsg = <<"Invalid oauth_doc_id: ", Value/binary>>,
            lager:debug("~s", [ErrorMsg]),
            jesse_error:handle_data_invalid('external_error', ErrorMsg, State)
    end.

maybe_check_param_stability_level('undefined', _ParamSL, State) ->
    State; %% SystemSL is undefined, skip checking
maybe_check_param_stability_level(_SystemSL, 'undefined', State) ->
    State; %% ParamSL is undefined, skip checking
maybe_check_param_stability_level(SystemSL, ParamSL, State) ->
    SystemSLInt = stability_level_to_int(SystemSL),
    ParamSLInt = stability_level_to_int(ParamSL),
    case check_param_stability_level(SystemSLInt, ParamSLInt) of
        valid ->
            State;
        invalid ->
            ErrorMsg = <<"Disallowed parameter, it has lower stability level (",
                         ParamSL/binary, ") than system's stability level (",
                         SystemSL/binary, ")">>,
            lager:debug("~s", [ErrorMsg]),
            jesse_error:handle_data_invalid('external_error', ErrorMsg, State)
    end.

check_param_stability_level(SystemSLInt, ParamSLInt) when ParamSLInt < SystemSLInt ->
    invalid;
check_param_stability_level(_SystemSLInt, _ParamSLInt) ->
    valid.

-spec stability_level_to_int(kz_term:ne_binary()) -> pos_integer().
stability_level_to_int(<<"stable">>) -> 3;
stability_level_to_int(<<"beta">>) -> 2;
stability_level_to_int(<<"alpha">>) -> 1.
