-define(CB_URL, "http://localhost:8000/v1/accounts").

-define(IBROWSE_HEADERS, [{"X-Auth-Token", "fixtures-test"}
			  ,{"Content-Type", "application/json"}
			  ,{"Accept", "application/json"}
			 ]).

-define(ACCT_JOBJ, {struct, [{<<"name">>, <<"fixtures account">>}
			     ,{<<"realm">>, wh_util:to_binary(net_adm:localhost())}
			    ]}).
