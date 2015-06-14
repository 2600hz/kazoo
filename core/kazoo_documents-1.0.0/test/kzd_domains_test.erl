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
-include_lib("whistle/include/wh_databases.hrl").

-define(WHITELABEL_DOMAIN, <<"2600hz.com">>).

-define(CNAM
        ,<<"{\"CNAM\":{\"portal.{{domain}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{domain}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

domains_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun format_host/1
       ,fun cnam/1
      ]
    }.

-define(DOMAINS_SCHEMA, <<"domains">>).
-define(HOSTS_SCHEMA, <<"domain_hosts">>).

init() ->
    CrossbarDir = code:lib_dir('crossbar'),

    DomainsSchema = load(CrossbarDir, ?DOMAINS_SCHEMA),
    DomainHostsSchema = load(CrossbarDir, ?HOSTS_SCHEMA),

    {DomainsSchema, DomainHostsSchema}.

load(AppPath, Filename) ->
    SchemaPath = filename:join([AppPath, "priv", "couchdb", "schemas"
                                ,<<Filename/binary, ".json">>
                               ]),
    {'ok', SchemaFile} = file:read_file(SchemaPath),
    wh_json:decode(SchemaFile).

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

cnam({DomainsSchema, DomainHostsSchema}) ->
    CNAM = wh_json:decode(?CNAM),

    Hosts = kzd_domains:cnam_hosts(CNAM),
    LoaderFun = fun(?HOSTS_SCHEMA) -> {'ok', DomainHostsSchema} end,

    [{"Validate cnam property in domains object"
      ,?_assertEqual({'ok', CNAM}
                     ,wh_json_schema:validate(DomainsSchema
                                              ,CNAM
                                              ,[{'schema_loader_fun', LoaderFun}]
                                             )
                    )
     }
     ,{"Validate list of hosts"
       ,?_assertEqual([<<"portal.{{domain}}">>
                       ,<<"api.{{domain}}">>
                      ]
                      ,Hosts
                     )
      }
     | validate_cnam_hosts(CNAM, Hosts)
    ].

validate_cnam_hosts(CNAM, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_cnam_host(CNAM, H) end
                ,Hosts
               )
     ).

validate_cnam_host(CNAM, Host) ->
    _HostMappings = kzd_domains:cnam_host_mappings(CNAM, Host),
    WhitelabelHost = kzd_domains:format_host(Host, ?WHITELABEL_DOMAIN),

    [{"Verify whitelabel host"
      ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?WHITELABEL_DOMAIN))
     }
    ].
