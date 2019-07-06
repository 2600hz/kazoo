-ifndef(KZD_CDRS_HRL).

-include("kz_documents.hrl").

-define(COLUMNS
       ,[{<<"id">>, fun col_id/2}
        ,{<<"call_id">>, fun col_call_id/2}
        ,{<<"caller_id_number">>, fun col_caller_id_number/2}
        ,{<<"caller_id_name">>, fun col_caller_id_name/2}
        ,{<<"callee_id_number">>, fun col_callee_id_number/2}
        ,{<<"callee_id_name">>, fun col_callee_id_name/2}
        ,{<<"duration_seconds">>, fun col_duration_seconds/2}
        ,{<<"billing_seconds">>, fun col_billing_seconds/2}
        ,{<<"timestamp">>, fun col_timestamp/2}
        ,{<<"hangup_cause">>, fun col_hangup_cause/2}
        ,{<<"other_leg_call_id">>, fun col_other_leg_call_id/2}
        ,{<<"owner_id">>, fun col_owner_id/2}
        ,{<<"to">>, fun col_to/2}
        ,{<<"from">>, fun col_from/2}
        ,{<<"direction">>, fun col_call_direction/2}
        ,{<<"request">>, fun col_request/2}
        ,{<<"authorizing_id">>, fun col_authorizing_id/2}
        ,{<<"cost">>, fun col_customer_cost/2}
         %% New fields
        ,{<<"dialed_number">>, fun col_dialed_number/2}
        ,{<<"calling_from">>, fun col_calling_from/2}
        ,{<<"datetime">>, fun col_pretty_print/2}
        ,{<<"unix_timestamp">>, fun col_unix_timestamp/2}
        ,{<<"rfc_1036">>, fun col_rfc1036/2}
        ,{<<"iso_8601">>, fun col_iso8601/2}
        ,{<<"iso_8601_combined">>, fun col_iso8601_combined/2}
        ,{<<"call_type">>, fun col_account_call_type/2}
        ,{<<"rate">>, fun col_rate/2}
        ,{<<"rate_name">>, fun col_rate_name/2}
        ,{<<"bridge_id">>, fun col_bridge_id/2}
        ,{<<"recording_url">>, fun col_recording_url/2}
        ,{<<"media_recordings">>, fun col_media_recordings/2}
        ,{<<"media_server">>, fun col_media_server/2}
        ,{<<"call_priority">>, fun col_call_priority/2}
        ,{<<"interaction_id">>, fun col_interaction_id/2}
        ]).

-define(COLUMNS_RESELLER
       ,[{<<"reseller_cost">>, fun col_reseller_cost/2}
        ,{<<"reseller_call_type">>, fun col_reseller_call_type/2}
        ]).

-define(KEY_CCV, <<"custom_channel_vars">>).
-define(KEY_UTC_OFFSET, <<"utc_offset">>).

-type csv_column_fun() :: fun((kz_json:object(), kz_time:gregorian_seconds()) -> kz_term:ne_binary()).

-define(SCHEMA, <<"cdrs">>).
-define(PVT_TYPE, <<"cdr">>).

-define(KZD_CDRS_HRL, 'true').
-endif.
