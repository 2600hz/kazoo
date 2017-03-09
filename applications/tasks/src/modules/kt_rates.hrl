-ifndef(KT_RATES_HRL).
-define(KT_RATES_HRL, 'true').

-define(DOC_FIELDS, [<<"account_id">>
                    ,<<"carrier">>
                    ,<<"description">>
                    ,<<"direction">>
                    ,<<"internal_rate_cost">>
                    ,<<"iso_country_code">>
                    ,<<"options">>
                    ,<<"prefix">>
                    ,<<"rate_cost">>
                    ,<<"rate_increment">>
                    ,<<"rate_minimum">>
                    ,<<"rate_name">>
                    ,<<"rate_nocharge_time">>
                    ,<<"rate_surcharge">>
                    ,<<"rate_version">>
                    ,<<"ratedeck_id">>
                    ,<<"routes">>
                    ,<<"weight">>
                    ]).

-define(MANDATORY_FIELDS, [<<"prefix">>
                          ,<<"rate_cost">>
                          ]).

-endif.
