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

-define(DOMAIN, <<"2600hz.com">>).

-define(CNAM
        ,<<"{\"CNAM\":{\"portal.{{domain}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{domain}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

-define(A_RECORD
        ,<<"{\"A\":{\"us-east.{{domain}}\":{\"name\":\"Primary Proxy\",\"zone\": \"us-east\",\"mapping\":[\"8.36.70.3\"]},\"us-central.{{domain}}\":{\"name\":\"Secondary Proxy\",\"zone\": \"us-central\",\"mapping\":[\"166.78.105.67\"]},\"us-west.{{domain}}\":{\"name\":\"Tertiary Proxy\",\"zone\": \"us-west\",\"mapping\":[\"8.30.173.3\"]}}}">>
       ).

domains_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun format_host/1
       ,fun cnam/1
       ,fun a_record/1
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
      ,?_assertEqual(<<"api.", (?DOMAIN)/binary>>
                     ,kzd_domains:format_host(<<"api.{{domain}}">>
                                              ,?DOMAIN
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
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{"Verify whitelabel host"
      ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
    ].


a_record({DomainsSchema, DomainHostsSchema}) ->
    A_RECORD = wh_json:decode(?A_RECORD),

    Hosts = kzd_domains:a_record_hosts(A_RECORD),
    LoaderFun = fun(?HOSTS_SCHEMA) -> {'ok', DomainHostsSchema} end,

    [{"Validate a_record property in domains object"
      ,?_assertEqual({'ok', A_RECORD}
                     ,wh_json_schema:validate(DomainsSchema
                                              ,A_RECORD
                                              ,[{'schema_loader_fun', LoaderFun}]
                                             )
                    )
     }
     ,{"Validate list of hosts"
       ,?_assertEqual([<<"us-east.{{domain}}">>
                       ,<<"us-central.{{domain}}">>
                       ,<<"us-west.{{domain}}">>
                      ]
                      ,Hosts
                     )
      }
     | validate_a_record_hosts(A_RECORD, Hosts)
    ].

validate_a_record_hosts(A_RECORD, Hosts) ->
    lists:flatten(
      lists:map(fun(H) -> validate_a_record_host(A_RECORD, H) end
                ,Hosts
               )
     ).

validate_a_record_host(A_RECORD, Host) ->
    _HostMappings = kzd_domains:a_record_host_mappings(A_RECORD, Host),
    WhitelabelHost = kzd_domains:format_host(Host, ?DOMAIN),

    [{"Verify whitelabel host"
      ,?_assert('nomatch' =/= binary:match(WhitelabelHost, ?DOMAIN))
     }
    ].
