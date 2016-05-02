-ifndef(TELETYPE_HRL).
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

-define(APP_NAME, <<"teletype">>).
-define(APP_VERSION, <<"4.0.0">> ).

-define(PVT_TYPE, kz_notification:pvt_type()).

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-define(CACHE_NAME, 'teletype_cache').

-type mime_tuples() :: [mimemail:mimetuple()].

%% {ContentType, Filename, Content}
-type attachment() :: {ne_binary(), ne_binary(), ne_binary()}.
-type attachments() :: [attachment()].

%% {ContentType, Content}
-type rendered_template() :: {ne_binary(), iolist()}.
-type rendered_templates() :: [rendered_template()].

%% {"to"/"cc"/etc, [Address,...]}
-type email_map() :: [{ne_binary(), ne_binaries()}].

-type init_param() :: {'macros', kz_json:object()} |
                      {'text', ne_binary()} |
                      {'html', ne_binary()} |
                      {'subject', ne_binary()} |
                      {'category', ne_binary()} |
                      {'friendly_name', ne_binary()} |
                      {'to', kz_json:object()} |
                      {'cc', kz_json:object()} |
                      {'bcc', kz_json:object()} |
                      {'from', ne_binary()} |
                      {'reply_to', api_binary()}.
-type init_params() :: [init_param(),...].

-define(TEXT_PLAIN, <<"text/plain">>).
-define(TEXT_HTML, <<"text/html">>).

-define(EMAIL_SPECIFIED, <<"specified">>).
-define(EMAIL_ORIGINAL, <<"original">>).
-define(EMAIL_ADMINS, <<"admins">>).

-define(CONFIGURED_EMAILS(Type, Addresses)
        ,kz_json:from_list(
           props:filter_undefined(
             [{<<"type">>, Type}
              ,{<<"email_addresses">>, Addresses}
             ])
          )
       ).
-define(CONFIGURED_EMAILS(Type), kz_json:from_list([{<<"type">>, Type}])).

-define(MACRO_VALUE(Key, Label, Name, Description)
        ,{Key
          ,kz_json:from_list([{<<"i18n_label">>, Label}
                              ,{<<"friendly_name">>, Name}
                              ,{<<"description">>, Description}
                             ])
         }).

-define(CALLER_ID_MACROS
        ,[?MACRO_VALUE(<<"caller_id.number">>, <<"caller_id_number">>, <<"Caller ID Number">>, <<"Number of the caller">>)
          ,?MACRO_VALUE(<<"caller_id.name">>, <<"caller_id_name">>, <<"Caller ID Name">>, <<"Name of the caller">>)
         ]).

-define(CALLEE_ID_MACROS
        ,[?MACRO_VALUE(<<"callee_id.number">>, <<"callee_id_number">>, <<"Callee ID Number">>, <<"Number of the callee">>)
          ,?MACRO_VALUE(<<"callee_id.name">>, <<"callee_id_name">>, <<"Callee ID Name">>, <<"Name of the callee">>)
         ]).

-define(DATE_CALLED_MACROS
        ,[?MACRO_VALUE(<<"date_called.utc">>, <<"date_called_utc">>, <<"Date (UTC)">>, <<"When was the voicemail left (UTC)">>)
          ,?MACRO_VALUE(<<"date_called.local">>, <<"date_called_local">>, <<"Date">>, <<"When was the voicemail left (Local time)">>)
         ]).

-define(FROM_MACROS
        ,[?MACRO_VALUE(<<"from.user">>, <<"from_user">>, <<"From User">>, <<"SIP From Username">>)
          ,?MACRO_VALUE(<<"from.realm">>, <<"from_realm">>, <<"From Realm">>, <<"SIP From Realm">>)
         ]).

-define(TO_MACROS
        ,[?MACRO_VALUE(<<"to.user">>, <<"to_user">>, <<"To User">>, <<"SIP To Username">>)
          ,?MACRO_VALUE(<<"to.realm">>, <<"to_realm">>, <<"To Realm">>, <<"SIP To Realm">>)
         ]).

-define(ACCOUNT_MACROS
        ,[?MACRO_VALUE(<<"account.name">>, <<"account_name">>, <<"Account Name">>, <<"Name of the account">>)
          ,?MACRO_VALUE(<<"account.realm">>, <<"account_realm">>, <<"Account Realm">>, <<"SIP Realm of the account">>)
          ,?MACRO_VALUE(<<"account.id">>, <<"account_id">>, <<"Account ID">>, <<"Account ID">>)
          ,?MACRO_VALUE(<<"account.language">>, <<"account_language">>, <<"Account Language">>, <<"Account Language">>)
          ,?MACRO_VALUE(<<"account.timezone">>, <<"account_timezone">>, <<"Account Timezone">>, <<"Account Timezone">>)
         ]).

-define(USER_MACROS
        ,[?MACRO_VALUE(<<"user.first_name">>, <<"user_first_name">>, <<"First Name">>, <<"First name of the user">>)
          ,?MACRO_VALUE(<<"user.last_name">>, <<"user_last_name">>, <<"Last Name">>, <<"Last name of the user">>)
          ,?MACRO_VALUE(<<"user.email">>, <<"user_email">>, <<"Email">>, <<"Email of the user">>)
          ,?MACRO_VALUE(<<"user.timezone">>, <<"user_timezone">>, <<"Timezone">>, <<"Timezone of the user">>)
          ,?MACRO_VALUE(<<"user.username">>, <<"username">>, <<"Username">>, <<"Username">>)
         ]).

-define(PORT_REQUEST_MACROS
        ,[?MACRO_VALUE(<<"port_request.carrier">>, <<"carrier">>, <<"Carrier">>, <<"Carrier">>)
          ,?MACRO_VALUE(<<"port_request.name">>, <<"name">>, <<"Name">>, <<"Name">>)
          ,?MACRO_VALUE(<<"port_request.bill_name">>, <<"bill_name">>, <<"Bill Name">>, <<"Name on the bill">>)
          ,?MACRO_VALUE(<<"port_request.bill_address">>, <<"bill_address">>, <<"Bill Address">>, <<"Address on the bill">>)
          ,?MACRO_VALUE(<<"port_request.bill_locality">>, <<"bill_locality">>, <<"Bill Locality">>, <<"City on the bill">>)
          ,?MACRO_VALUE(<<"port_request.bill_region">>, <<"bill_region">>, <<"Bill Region">>, <<"Region on the bill">>)
          ,?MACRO_VALUE(<<"port_request.bill_postal_code">>, <<"bill_postal_code">>, <<"Bill Postal Code">>, <<"Postal Code on the bill">>)
          ,?MACRO_VALUE(<<"port_request.transfer_date">>, <<"transfer_date">>, <<"Transfer Date">>, <<"Transfer Date">>)
          ,?MACRO_VALUE(<<"port_request.numbers">>, <<"numbers">>, <<"Numbers">>, <<"Numbers">>)
          ,?MACRO_VALUE(<<"port_request.comments">>, <<"comments">>, <<"Comments">>, <<"Comments">>)
          ,?MACRO_VALUE(<<"port_request.scheduled_date">>, <<"scheduled_date">>, <<"Scheduled Date">>, <<"Scheduled Date">>)
         ]).

-define(SYSTEM_MACROS
        ,[?MACRO_VALUE(<<"system.hostname">>, <<"system_hostname">>, <<"Hostname">>, <<"Hostname of system generating the email">>)
         ]
       ).

-define(DEFAULT_CALL_MACROS
        ,?CALLER_ID_MACROS
        ++ ?CALLEE_ID_MACROS
        ++ ?DATE_CALLED_MACROS
        ++ ?FROM_MACROS
        ++ ?TO_MACROS
       ).

-record(email_receipt, {to :: ne_binaries() | ne_binary()
                        ,from :: ne_binary()
                        ,call_id :: ne_binary()
                        ,timestamp :: gregorian_seconds()
                       }).
-type email_receipt() :: #email_receipt{}.

-define(TELETYPE_HRL, 'true').
-endif.
