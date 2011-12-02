%%%%-------------------------------------------------------------------
%%% @authors Edouard Swiac <edouard@2600hz.com>
%%%          James Aimonetti <james@2600hz.org>
%%%
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Front-end to wh_json_validator for fetching the schema object and
%%% calling the validator
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(crossbar_schema).

-export([do_validate/2]).

-include("../include/crossbar.hrl").
-include_lib("eunit/include/eunit.hrl").

-type ok() :: {'ok', []}.
-type errors() :: {'errors', [{ne_binary(), ne_binary()},...]}.
-type result() :: ok() | errors().

-export_type([result/0, ok/0, errors/0]).

-define(OK, {ok, []}).
-define(ERRORS(E), {errors, E}).

%%--------------------------------------------------------------------
%% @doc
%% Performs the validation of a JSON structure against a schema stored
%% in DB as a couch doc
%% @end
%%--------------------------------------------------------------------
-spec do_validate/2 :: (json_object() | binary() | number() | list(), atom() | ne_binary()) -> result().
do_validate(JObj, SchemaName) ->
    case couch_mgr:open_doc(?SCHEMAS_DB, wh_util:to_binary(SchemaName)) of
	{ok, Schema} ->
	    validate(JObj, Schema);
	{error, _} ->
	    ?OK
    end.

-spec validate/2 :: (json_object() | binary() | number() | list(), json_object()) -> result().
validate(JObj, Schema) ->
    case wh_json:is_json_object(JObj) andalso wh_json_validator:is_valid_object(JObj, Schema) of
	true -> ?OK;
	false ->
	    %% JObj is actually a simple value (number, string, etc)
	    case wh_json_validator:is_valid_attribute(JObj, Schema) of
		true -> ?OK;
		Errors -> ?ERRORS(Errors)
	    end;
	Errors -> ?ERRORS(Errors)
    end.
