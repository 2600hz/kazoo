-ifndef(KNM_VITELITY_HRL).

-define(KNM_VITELITY_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".vitelity">>).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(PREFIX_SEARCH_CMD, <<"listnpa">>).
-define(PREFIX_SEARCH_RESP, knm_util:fixture("vitelity_listnpa.xml")).

-define(NUMBER_SEARCH_CMD, <<"listnpanxx">>).
-define(NUMBER_SEARCH_RESP, knm_util:fixture("vitelity_listnpanxx.xml")).

-define(TOLLFREE_SEARCH_CMD, <<"listtollfree">>).
-define(TOLLFREE_SEARCH_RESP, knm_util:fixture("vitelity_listtollfree.xml")).

-define(VITELITY_URI, <<"http://api.vitelity.net/api.php">>).

-else.

-define(VITELITY_URI
        ,whapps_config:get(?KNM_VITELITY_CONFIG_CAT
                           ,<<"api_uri">>
                           ,<<"http://api.vitelity.net/api.php">>
                          )
       ).
-endif.

%% <<"yes">> or <<"no">>
-type yes_or_no() :: <<_:16>> | <<_:24>>.
-type state_two_letters() :: <<_:16>>.

-type qs_option() :: {'did', ne_binary()} |
                     {'name', ne_binary()} |
                     {'cmd', ne_binary()} |
                     {'xml', yes_or_no()} |
                     {'routesip', ne_binary()} |
                     {'npanxx', ne_binary()} |
                     {'type', ne_binary()} |
                     {'withrates', yes_or_no()} |
                     {'provider', ne_binary()} |
                     {'cnam', yes_or_no()} |
                     {'address', ne_binary()} |
                     {'city', ne_binary()} |
                     {'state', state_two_letters()} |
                     {'zip', ne_binary() | integer()} |
                     {'ratecenter', ne_binary()} |
                     {'smsonly', yes_or_no()} |
                     {'login', ne_binary()} |
                     {'pass', ne_binary()}.
-type qs_options() :: [qs_option()].

-type query_option() :: {'qs', qs_options()} |
                        {'uri', ne_binary()}.
-type query_options() :: [query_option()].

-define(KNM_VITELITY_HRL, 'true').
-endif.
