-ifndef(KNM_VITELITY_HRL).

-define(KNM_VITELITY_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".vitelity">>).

-ifdef(TEST).
-define(PREFIX_SEARCH_CMD, <<"listnpa">>).
-define(PREFIX_SEARCH_RESP, list_to_binary(knm_util:fixture("vitelity_listnpa.xml"))).

-define(NUMBER_SEARCH_CMD, <<"listnpanxx">>).
-define(NUMBER_SEARCH_RESP, list_to_binary(knm_util:fixture("vitelity_listnpanxx.xml"))).

-define(TOLLFREE_SEARCH_CMD, <<"listtollfree">>).
-define(TOLLFREE_SEARCH_RESP, list_to_binary(knm_util:fixture("vitelity_listtollfree.xml"))).
-endif.

%% <<"yes">> or <<"no">>
-type yes_or_no() :: <<_:16>> | <<_:24>>.
-type state_two_letters() :: <<_:16>>.

-type qs_option() :: {'did', kz_term:ne_binary()} |
                     {'name', kz_term:ne_binary()} |
                     {'cmd', kz_term:ne_binary()} |
                     {'xml', yes_or_no()} |
                     {'routesip', kz_term:ne_binary()} |
                     {'npanxx', kz_term:ne_binary()} |
                     {'type', kz_term:ne_binary()} |
                     {'withrates', yes_or_no()} |
                     {'provider', kz_term:ne_binary()} |
                     {'cnam', yes_or_no()} |
                     {'address', kz_term:ne_binary()} |
                     {'city', kz_term:ne_binary()} |
                     {'state', state_two_letters()} |
                     {'zip', kz_term:ne_binary() | integer()} |
                     {'ratecenter', kz_term:ne_binary()} |
                     {'smsonly', yes_or_no()} |
                     {'login', kz_term:ne_binary()} |
                     {'pass', kz_term:ne_binary()}.
-type qs_options() :: [qs_option()].

-type query_option() :: {'qs', qs_options()} |
                        {'uri', kz_term:ne_binary()}.
-type query_options() :: [query_option()].

-define(KNM_VITELITY_HRL, 'true').
-endif.
