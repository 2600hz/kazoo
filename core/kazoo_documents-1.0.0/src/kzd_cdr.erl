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
         ,duration_s/1, duration_s/2
         ,ringing_s/1, ringing_s/2
         ,billing_s/1, billing_s/2
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(KEY_CALL_ID, <<"call_id">>).
-define(KEY_OTHER_LEG_CALL_ID, <<"other_leg_call_id">>).
-define(KEY_CALL_DIRECTION, [<<"call_direction">>, <<"direction">>]).
-define(KEY_TIMESTAMP, <<"timestamp">>).

-define(KEY_DURATION_S, <<"duration_seconds">>).
-define(KEY_RINGING_S, <<"ringing_seconds">>).
-define(KEY_BILLING_S, <<"billing_seconds">>).

-spec call_id(doc()) -> api_binary().
call_id(CDR) ->
    wh_json:get_value(?KEY_CALL_ID, CDR).

-spec other_leg_call_id(doc()) -> api_binary().
other_leg_call_id(CDR) ->
    wh_json:get_value(?KEY_OTHER_LEG_CALL_ID, CDR).

-spec call_direction(doc()) -> api_binary().
call_direction(CDR) ->
    wh_json:get_first_defined(?KEY_CALL_DIRECTION, CDR).

-spec timestamp(doc()) -> gregorian_seconds() | 'undefined'.
timestamp(CDR) ->
    wh_json:get_integer_value(?KEY_TIMESTAMP, CDR).

-spec duration_s(doc()) -> api_integer().
-spec duration_s(doc(), Default) -> integer() | Default.
duration_s(CDR) ->
    duration_s(CDR, 'undefined').
duration_s(CDR, Default) ->
    wh_json:get_integer_value(?KEY_DURATION_S, CDR, Default).

-spec ringing_s(doc()) -> api_integer().
-spec ringing_s(doc(), Default) -> integer() | Default.
ringing_s(CDR) ->
    ringing_s(CDR, 'undefined').
ringing_s(CDR, Default) ->
    wh_json:get_integer_value(?KEY_RINGING_S, CDR, Default).

-spec billing_s(doc()) -> api_integer().
-spec billing_s(doc(), Default) -> integer() | Default.
billing_s(CDR) ->
    billing_s(CDR, 'undefined').
billing_s(CDR, Default) ->
    wh_json:get_integer_value(?KEY_BILLING_S, CDR, Default).
