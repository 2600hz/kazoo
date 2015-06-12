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

-define(CNAM
        ,<<"{\"CNAM\":{\"portal.{{whitelabel_domain}}\":{\"name\":\"Web GUI\",\"mapping\":[\"ui.zswitch.net\"]},\"api.{{whitelabel_domain}}\":{\"name\":\"API\",\"mapping\":[\"api.zswitch.net\"]}}}">>
       ).

domains_test_() ->
    {'foreach'
     ,fun init/0
     ,fun stop/1
     ,[fun cnam/1]
    }.

init() ->
    CrossbarDir = code:lib_dir('crossbar'),
    DomainsSchemaPath = filename:join([CrossbarDir, "priv", "couchdb", "schemas", "domains.json"]),
    {'ok', DomainsSchemaFile} = file:read_file(DomainsSchemaPath),
    wh_json:decode(DomainsSchemaFile).

stop(_) -> 'ok'.

cnam(DomainsSchema) ->
    CNAM = wh_json:decode(?CNAM),
    [{"Validate cnam property in domains object"
      ,?_assertEqual({'ok', CNAM}, wh_json_schema:validate(DomainsSchema, CNAM))
     }
    ].
