%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% CDR document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_cdr).

-export([call_id/1
         ,other_leg_call_id/1
         ,call_direction/1
         ,timestamp/1
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_CALL_ID, <<"call_id">>).
-define(KEY_OTHER_LEG_CALL_ID, <<"other_leg_call_id">>).
-define(KEY_CALL_DIRECTION, <<"call_direction">>).
-define(KEY_TIMESTAMP, <<"timestamp">>).

-spec call_id(doc()) -> api_binary().
call_id(CDR) ->
    wh_json:get_value(?KEY_CALL_ID, CDR).

-spec other_leg_call_id(doc()) -> api_binary().
other_leg_call_id(CDR) ->
    wh_json:get_value(?KEY_OTHER_LEG_CALL_ID, CDR).

-spec call_direction(doc()) -> api_binary().
call_direction(CDR) ->
    wh_json:get_value(?KEY_CALL_DIRECTION, CDR).

-spec timestamp(doc()) -> gregorian_seconds() | 'undefined'.
timestamp(CDR) ->
    wh_json:get_value(?KEY_TIMESTAMP, CDR).
