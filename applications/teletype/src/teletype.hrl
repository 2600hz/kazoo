-ifndef(TELETYPE_HRL).
-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-define(APP_NAME, <<"teletype">>).
-define(APP_VERSION, <<"0.0.1">> ).

-define(PVT_TYPE, <<"notification">>).

-define(NOTIFY_CONFIG_CAT, <<"notify">>).

-type mime_tuples() :: [mimemail:mimetuple(),...] | [].

%% {ContentType, Filename, Content}
-type attachment() :: {ne_binary(), ne_binary(), ne_binary()}.
-type attachments() :: [attachment(),...] | [].

-type init_param() :: {'macros', wh_json:object()} |
                      {'text', ne_binary()} |
                      {'html', ne_binary()} |
                      {'subject', ne_binary()} |
                      {'category', ne_binary()} |
                      {'friendly_name', ne_binary()} |
                      {'to', wh_json:object()} |
                      {'cc', wh_json:object()} |
                      {'bcc', wh_json:object()} |
                      {'from', ne_binary()} |
                      {'reply_to', api_binary()}.
-type init_params() :: [init_param(),...].

-define(TEXT_PLAIN, <<"text/plain">>).
-define(TEXT_HTML, <<"text/html">>).

-define(CONFIGURED_EMAILS(Type, Addresses)
        ,wh_json:from_list(
           props:filter_undefined(
             [{<<"type">>, Type}
              ,{<<"email_addresses">>, Addresses}
             ])
          )
       ).
-define(CONFIGURED_EMAILS(Type), wh_json:from_list([{<<"type">>, Type}])).

-define(MACRO_VALUE(Key, Label, Name, Description)
        ,{Key
          ,wh_json:from_list([{<<"i18n_label">>, Label}
                              ,{<<"friendly_name">>, Name}
                              ,{<<"description">>, Description}
                             ])
         }).

-define(TELETYPE_HRL, 'true').
-endif.
