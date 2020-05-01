-ifndef(KZD_CDRS_HRL).

-include("kz_documents.hrl").

-define(COLUMNS
       ,[{<<"id">>, fun col_id/3}
        ,{<<"call_id">>, fun col_call_id/3}
        ,{<<"caller_id_number">>, fun col_caller_id_number/3}
        ,{<<"caller_id_name">>, fun col_caller_id_name/3}
        ,{<<"callee_id_number">>, fun col_callee_id_number/3}
        ,{<<"callee_id_name">>, fun col_callee_id_name/3}
        ,{<<"duration_seconds">>, fun col_duration_seconds/3}
        ,{<<"billing_seconds">>, fun col_billing_seconds/3}
        ,{<<"timestamp">>, fun col_timestamp/3}
        ,{<<"hangup_cause">>, fun col_hangup_cause/3}
        ,{<<"other_leg_call_id">>, fun col_other_leg_call_id/3}
        ,{<<"owner_id">>, fun col_owner_id/3}
        ,{<<"to">>, fun col_to/3}
        ,{<<"from">>, fun col_from/3}
        ,{<<"direction">>, fun col_call_direction/3}
        ,{<<"request">>, fun col_request/3}
        ,{<<"authorizing_id">>, fun col_authorizing_id/3}
        ,{<<"cost">>, fun col_customer_cost/3}
         %% New fields
        ,{<<"dialed_number">>, fun col_dialed_number/3}
        ,{<<"calling_from">>, fun col_calling_from/3}
        ,{<<"datetime">>, fun col_pretty_print/3}
        ,{<<"unix_timestamp">>, fun col_unix_timestamp/3}
        ,{<<"rfc_1036">>, fun col_rfc1036/3}
        ,{<<"iso_8601">>, fun col_iso8601/3}
        ,{<<"iso_8601_combined">>, fun col_iso8601_combined/3}
        ,{<<"call_type">>, fun col_account_call_type/3}
        ,{<<"rate">>, fun col_rate/3}
        ,{<<"rate_name">>, fun col_rate_name/3}
        ,{<<"bridge_id">>, fun col_bridge_id/3}
        ,{<<"recording_url">>, fun col_recording_url/3}
        ,{<<"media_recordings">>, fun col_media_recordings/3}
        ,{<<"media_server">>, fun col_media_server/3}
        ,{<<"call_priority">>, fun col_call_priority/3}
        ,{<<"interaction_id">>, fun col_interaction_id/3}
        ]).

-define(COLUMNS_RESELLER
       ,[{<<"reseller_cost">>, fun col_reseller_cost/3}
        ,{<<"reseller_call_type">>, fun col_reseller_call_type/3}
        ]).

-define(KEY_CCV, <<"custom_channel_vars">>).
-define(KEY_UTC_OFFSET, <<"utc_offset">>).

-type csv_column_fun() :: fun((kz_json:object(), kz_time:gregorian_seconds(), kz_term:api_ne_binary()) -> kz_term:ne_binary() | kz_term:ne_binaries()).

-define(SCHEMA, <<"cdrs">>).
-define(PVT_TYPE, <<"cdr">>).

-define(KZD_CDRS_HRL, 'true').
-endif.
