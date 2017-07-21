%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2017, 2600Hz INC
%%% @doc
%%% Module for extending schema validation
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_json_schema_extensions).

-export([extra_validator/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(INVALID_STORAGE_ATTACHMENT_REFERENCE(R), <<"invalid reference '", R/binary, "' to attachments">>).
-define(INVALID_STORAGE_CONNECTION_REFERENCE(R), <<"invalid reference '", R/binary, "' to connections">>).

-spec extra_validator(jesse:json_term(), jesse_state:state()) -> jesse_state:state().
extra_validator(Value, State) ->
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
    Module = jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, undefined),
    lager:debug("validating metaflow action '~s' with data ~p", [Module, Value]),
    validate_module_data(<<"metaflow.", Module/binary>>, Value, State);
extra_validation(<<"metaflow.module">>, Value, State) ->
    lager:debug("validating metaflow action '~s'", [Value]),
    Schema = <<"metaflow.", Value/binary>>,
    State1 = jesse_state:resolve_ref(State, Schema),
    State2 = case jesse_state:get_current_schema_id(State1) of
                 Schema -> State1;
                 _OtherSchema -> jesse_error:handle_data_invalid(external_error, <<"unable to find metaflow schema for module ", Value/binary>>, State)
             end,
    jesse_state:undo_resolve_ref(State2, State);
extra_validation(<<"callflows.action.data">>, Value, State) ->
    JObj = jesse_state:get_current_value(State),
    [_Data | Path] = jesse_state:get_current_path(State),
    case jesse_json_path:path(lists:reverse([<<"module">> | Path]), JObj, undefined) of
        undefined -> State;
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
