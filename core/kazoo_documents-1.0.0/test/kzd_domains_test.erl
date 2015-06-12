%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%% Account document
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_domains_test).

-include_lib("eunit/include/eunit.hrl").

-define(WHITELABEL_DOMAIN, <<"2600hz.com">>).

-define(CNAM
        ,<<"{\"CNAM\":{\"portal.{{whitelabel_domain}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{whitelabel_domain}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

domains_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun format_host/1
       ,fun cnam/1
      ]
    }.

init() ->
    CrossbarDir = code:lib_dir('crossbar'),
    DomainsSchemaPath = filename:join([CrossbarDir, "priv", "couchdb", "schemas", "domains.json"]),
    {'ok', DomainsSchemaFile} = file:read_file(DomainsSchemaPath),
    wh_json:decode(DomainsSchemaFile).

stop(_) -> 'ok'.

format_host(_) ->
    [{"Verify host replacement happens"
      ,?_assertEqual(<<"api.", (?WHITELABEL_DOMAIN)/binary>>
                     ,kzd_domains:format_host(<<"api.{{domain}}">>
                                              ,?WHITELABEL_DOMAIN
                                             )
                    )
     }
    ].

cnam(DomainsSchema) ->
    CNAM = wh_json:decode(?CNAM),

    Hosts = kzd_domains:cnam_hosts(CNAM),

    [{"Validate cnam property in domains object"
      ,?_assertEqual({'ok', CNAM}, wh_json_schema:validate(DomainsSchema, CNAM))
     }
     ,{"Validate list of hosts"
       ,?_assertEqual([<<"portal.{{whitelabel_domain}}">>
                       ,<<"api.{{whitelabel_domain}}">>
                      ]
                      ,Hosts
                     )
      }
     | validate_hosts(CNAM, Hosts)
    ].

validate_hosts(CNAM, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_host(CNAM, H) end
                ,Hosts
               )
     ).

validate_host(CNAM, Host) ->
    _HostMappings = kzd_domains:cnam_host_mappings(CNAM, Host),
    WhitelabelHost = kzd_domains:format_host(Host, ?WHITELABEL_DOMAIN),
    [{"Verify whitelabel host"
      ,?_assertEqual(WhitelabelHost, WhitelabelHost)
     }
    ].
