%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc One-off for building pqc_cb_* modules that have CRUD-y APIs
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_builder).

-export([process/0, process_app/1, process_module/1
        ,to_pqc_mods/0 ,to_pqc_mod/1
        ]).

-include_lib("kazoo_ast/include/kz_ast.hrl").
-include_lib("kazoo_ast/src/kz_ast.hrl").
-include_lib("kazoo_web/include/kazoo_web.hrl").

-define(DOLLAR_SIGN, 36). % formatter (at least in CI) still barfs on $ in regex

-type acc() :: {{module(), atom(), arity()}, dict:dict()}.

-define(CONFIG, [{'after_module', fun print_dot/2}
                ,{'module', fun start_module/2}
                ,{'function', fun handle_allowed_methods/3}
                ,{'clause', fun handle_allowed_arity/4}
                ,{'expression', fun handle_verbs/2}
                ,{'accumulator', {{'undefined', 'undefined', 0}, dict:new()}} %% {{module, function, arity}, [{mfa, verbs}]}
                ]).

-spec to_pqc_mods() -> 'ok'.
to_pqc_mods() ->
    Dict = process(),
    build_modules(Dict).

-spec to_pqc_mod(module()) -> 'ok'.
to_pqc_mod(Module) ->
    Dict = process_module(Module),
    build_modules(Dict).

-record(src_module, {module :: module()
                    ,exports = [] :: iolist()
                    ,functions = [] :: iolist()
                    }).

build_modules(Dict) ->
    List = lists:keysort(1, dict:to_list(Dict)),
    SrcModule = lists:foldl(fun build_module/2, #src_module{}, List),
    write_source(SrcModule).

build_module({{M, _F, A}, Verbs}, #src_module{module=M}=Acc) ->
    add_exports_and_functions(M, A, Verbs, Acc);
build_module({{NewM, _, _}, _}=NewMFA, Acc) ->
    write_source(Acc),
    build_module(NewMFA, #src_module{module=NewM}).

write_source(#src_module{module='undefined'}) -> 'ok';
write_source(#src_module{module=Module
                        ,exports=[_S, _C, _F, _U, _P, _D]
                        }=SrcModule) ->
    PQCFile = module_to_pqc_filename(Module),
    'ok' = file:write_file(PQCFile, src_module_to_iolist(SrcModule)),
    io:format("wrote ~s~n", [PQCFile]);
write_source(#src_module{module=_M}) -> 'ok'.

src_module_to_iolist(#src_module{module=Module
                                ,exports=Exports
                                ,functions=APIFunctions
                                }
                    ) ->
    %% create base exports/functions for seq/cleanup
    [src_header(Module)
    ,lists:reverse(Exports)
    ,"\n"
    ,seq_exports(Module)
    ,"\n"
     "-include(\"kazoo_proper.hrl\").\n\n"
     "-define(ACCOUNT_NAMES, [<<?MODULE_STRING>>]).\n\n"
    ,lists:reverse(APIFunctions)
    ,"\n"
    ,url_functions(Module)
    ,"\n"
    ,seq_functions(Module)
    ].

module_to_pqc_filename(Module) ->
    SrcFile = module_to_source_file(Module),
    filename:join([code:lib_dir('kazoo_proper', 'src'), SrcFile]).

module_to_source_file(Module) ->
    Bin = kz_term:to_binary(Module),
    <<"pqc_", Bin/binary, ".erl">>.

src_header(Module) ->
    File = module_to_source_file(Module),
    PQC = filename:basename(File, <<".erl">>),
    ["-" "module(", PQC, ").\n\n"].

add_exports_and_functions(M, A, Verbs, SrcModule) ->
    lists:foldl(fun(F, Acc) -> F(M, A, Verbs, Acc) end
               ,SrcModule
               ,[fun add_exports/4
                ,fun add_functions/4
                ]
               ).

add_functions(M, A, Verbs, #src_module{functions=Functions}=SrcModule) ->
    Fs = lists:foldl(fun(Verb, Acc) -> add_function(M, A, Verb, Acc) end
                    ,Functions
                    ,Verbs
                    ),
    SrcModule#src_module{functions=Fs}.

add_function(M, 0, ?HTTP_GET, Functions) ->
    [["-spec summary(pqc_cb_api:state(), kz_term:ne_binary()) -> pqc_cb_api:response().\n"
      "summary(API, AccountId) ->\n"
      "    pqc_cb_crud:summary(API, ", plural(M), "_url(AccountId)).\n\n"
     ]
     | Functions
    ];
add_function(M, 0, ?HTTP_PUT, Functions) ->
    Singular = kz_binary:ucfirst(singular(M)),
    Plural = plural(M),
    [["-spec create(pqc_cb_api:state(), kz_term:ne_binary(), kzd_", Plural, ":doc()) -> pqc_cb_api:response().\n"
      "create(API, AccountId, ", Singular, "JObj) ->\n"
      "    Envelope = pqc_cb_api:create_envelope(", Singular, "JObj),\n"
      "    pqc_cb_crud:create(API, ", Plural, "_url(AccountId), Envelope).\n\n"
     ]
     | Functions
    ];
add_function(M, 1, ?HTTP_GET, Functions) ->
    Singular = singular(M),
    UCSingular = kz_binary:ucfirst(Singular),
    [["-spec fetch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().\n"
      "fetch(API, AccountId, ", UCSingular, "Id) ->\n"
      "    pqc_cb_crud:fetch(API, ", Singular, "_url(AccountId, ", UCSingular, "Id)).\n\n"
     ]
     | Functions
    ];
add_function(M, 1, ?HTTP_POST, Functions) ->
    Singular = singular(M),
    UCSingular = kz_binary:ucfirst(Singular),
    [["-spec update(pqc_cb_api:state(), kz_term:ne_binary(), kzd_", Singular, ":doc()) -> pqc_cb_api:response().\n"
      "update(API, AccountId, ", UCSingular, "JObj) ->\n"
      "    Envelope = pqc_cb_api:create_envelope(", UCSingular, "JObj),\n"
      "    pqc_cb_crud:update(API, ", Singular, "_url(AccountId, kz_doc:id(", UCSingular, "JObj)), Envelope).\n\n"
     ]
     | Functions
    ];
add_function(M, 1, ?HTTP_PATCH, Functions) ->
    Singular = singular(M),
    UCSingular = kz_binary:ucfirst(Singular),
    [["-spec patch(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) -> pqc_cb_api:response().\n"
      "patch(API, AccountId, ", UCSingular, "Id, PatchJObj) ->\n"
      "    Envelope = pqc_cb_api:create_envelope(PatchJObj),\n"
      "    pqc_cb_crud:patch(API, ", Singular, "_url(AccountId, ", UCSingular, "Id), Envelope).\n\n"
     ]
     | Functions
    ];
add_function(M, 1, ?HTTP_DELETE, Functions) ->
    Singular = singular(M),
    UCSingular = kz_binary:ucfirst(Singular),
    [["-spec delete(pqc_cb_api:state(), kz_term:ne_binary(), kz_term:ne_binary()) -> pqc_cb_api:response().\n"
      "delete(API, AccountId, ", UCSingular, "Id) ->\n"
      "    pqc_cb_crud:delete(API, ", Singular, "_url(AccountId, ", UCSingular, "Id)).\n\n"
     ]
     | Functions
    ];
add_function(_M, _Arity, _Verb, Functions) ->
    Functions.

url_functions(Module) ->
    Plural = plural(Module),
    Singular = singular(Module),
    UCSingular = kz_binary:ucfirst(Singular),

    ["-spec ", Plural, "_url(kz_term:ne_binary()) -> string().\n"
    ,Plural, "_url(AccountId) ->\n"
    ,"    pqc_cb_crud:collection_url(AccountId, <<\"", Plural, "\">>).\n"
    ,"\n"
    ,"-spec ", Singular, "_url(kz_term:ne_binary(), kz_term:ne_binary()) -> string().\n"
    ,Singular, "_url(AccountId, ", UCSingular, "Id) ->\n"
    ,"    pqc_cb_crud:entity_url(AccountId, <<\"", Plural, "\">>, ", UCSingular, "Id).\n\n"
    ].

seq_exports(Module) ->
    Singular = singular(Module),
    ["-export([seq/0\n"
     "        ,cleanup/0\n"
     "        ,new_", Singular, "/0\n"
     "        ]).\n"
    ].

seq_functions(Module) ->
    Singular = singular(Module),
    UCSingular = kz_binary:ucfirst(Singular),
    Plural = plural(Module),

    ["-spec seq() -> 'ok'.\n"
     "seq() ->\n"
     "    API = pqc_cb_api:init_api(['crossbar'], ['", kz_term:to_list(Module), "']),\n"
     "    AccountId = create_account(API),\n"
     "\n"
     "    EmptySummaryResp = summary(API, AccountId),\n"
     "    lager:info(\"empty summary resp: ~s\", [EmptySummaryResp]),\n"
     "    [] = kz_json:get_list_value(<<\"data\">>, kz_json:decode(EmptySummaryResp)),\n"
     "\n"
     "    ", UCSingular, "JObj = new_", Singular, "(),\n"
     "    CreateResp = create(API, AccountId, ", UCSingular, "JObj),\n"
     "    lager:info(\"created ", Singular, " ~s\", [CreateResp]),\n"
     "    Created", UCSingular, " = kz_json:get_json_value(<<\"data\">>, kz_json:decode(CreateResp)),\n"
     "    ", UCSingular, "Id = kz_doc:id(Created", UCSingular, "),\n"
     "\n"
     "    Patch = kz_json:from_list([{<<\"custom\">>, <<\"value\">>}]),\n"
     "    PatchResp = patch(API, AccountId, ", UCSingular, "Id, Patch),\n"
     "    lager:info(\"patched to ~s\", [PatchResp]),\n"
     "\n"
     "    SummaryResp = summary(API, AccountId),\n"
     "    lager:info(\"summary resp: ~s\", [SummaryResp]),\n"
     "    [Summary", UCSingular, "] = kz_json:get_list_value(<<\"data\">>, kz_json:decode(SummaryResp)),\n"
     "    ", UCSingular, "Id = kz_doc:id(Summary", UCSingular, "),\n"
     "\n"
     "    DeleteResp = delete(API, AccountId, ", UCSingular, "Id),\n"
     "    lager:info(\"delete resp: ~s\", [DeleteResp]),\n"
     "\n"
     "    EmptyAgain = summary(API, AccountId),\n"
     "    lager:info(\"empty summary resp: ~s\", [EmptyAgain]),\n"
     "    [] = kz_json:get_list_value(<<\"data\">>, kz_json:decode(EmptyAgain)),\n"
     "\n"
     "    cleanup(API),\n"
     "    lager:info(\"FINISHED ", kz_term:to_upper_binary(Singular), " SEQ\").\n"
     "\n"
     "-spec cleanup() -> 'ok'.\n"
     "cleanup() ->\n"
     "    _ = pqc_cb_accounts:cleanup_accounts(?ACCOUNT_NAMES),\n"
     "    cleanup_system().\n"
     "\n"
     "cleanup(API) ->\n"
     "    lager:info(\"CLEANUP TIME, EVERYBODY HELPS\"),\n"
     "    _ = pqc_cb_accounts:cleanup_accounts(API, ?ACCOUNT_NAMES),\n"
     "    _ = pqc_cb_api:cleanup(API),\n"
     "    cleanup_system().\n"
     "\n"
     "cleanup_system() -> 'ok'.\n"
     "\n"
     "-spec create_account(pqc_cb_api:state()) -> kz_term:ne_binary().\n"
     "create_account(API) ->\n"
     "    AccountResp = pqc_cb_accounts:create_account(API, hd(?ACCOUNT_NAMES)),\n"
     "    lager:info(\"created account: ~s\", [AccountResp]),\n"
     "\n"
     "    kz_json:get_ne_binary_value([<<\"data\">>, <<\"id\">>], kz_json:decode(AccountResp)).\n"
     "\n"
     "-spec new_", Singular, "() -> kzd_", Plural, ":doc().\n"
     "new_", Singular, "() ->\n"
     "    kz_doc:public_fields(kzd_", Plural, ":new()).\n"
    ].

singular(<<"cb_", Module/binary>>) -> singular(Module);
singular(<<Module/binary>>) ->
    Endpoint = maybe_remove_version(Module),
    maybe_remove_plural(Endpoint);
singular('cb_clicktocall') -> <<"clicktocall">>;
singular('cb_connectivity') -> <<"connectivity">>;
singular(Module) -> singular(kz_term:to_binary(Module)).

maybe_remove_plural(<<Endpoint/binary>>) ->
    case re:replace(Endpoint, <<"s", ?DOLLAR_SIGN>>, <<>>) of
        [Singular, _] -> Singular;
        Endpoint -> Endpoint
    end.

plural(<<"cb_", Module/binary>>) -> plural(Module);
plural(<<Module/binary>>) -> maybe_remove_version(Module);
plural('cb_clicktocall') -> <<"clicktocall">>;
plural('cb_connectivity') -> <<"connectivity">>;
plural(Module) -> plural(kz_term:to_binary(Module)).

maybe_remove_version(<<Endpoint/binary>>) ->
    Split = binary:split(Endpoint, <<"_">>, ['global']),
    case lists:last(Split) of
        <<"v2">> -> kz_binary:join(lists:droplast(Split), $_);
        _ -> Endpoint
    end.

add_exports(_M, A, Verbs, #src_module{exports=Exports}=SrcModule) ->
    Exs = lists:foldl(fun(Verb, Acc) -> add_export(A, Verb, Acc) end
                     ,Exports
                     ,Verbs
                     ),
    SrcModule#src_module{exports=Exs}.

add_export(0, ?HTTP_GET, Exports) ->
    ["-export([summary/2]).\n" | Exports];
add_export(0, ?HTTP_PUT, Exports) ->
    ["-export([create/3]).\n" | Exports];
add_export(1, ?HTTP_GET, Exports) ->
    ["-export([fetch/3]).\n" | Exports];
add_export(1, ?HTTP_POST, Exports) ->
    ["-export([update/3]).\n" | Exports];
add_export(1, ?HTTP_PATCH, Exports) ->
    ["-export([patch/4]).\n" | Exports];
add_export(1, ?HTTP_DELETE, Exports) ->
    ["-export([delete/3]).\n" | Exports];
add_export(_Arity, _Verb, Exports) ->
    Exports.

-spec process() -> dict:dict().
process() ->
    io:format("processing pqc CRUD "),
    {_MFA, Dict} = kazoo_ast:walk_project(?CONFIG),
    io:format(" done~n"),
    Dict.

-spec process_app(atom()) -> dict:dict().
process_app(App) ->
    {_MFA, Dict} = kazoo_ast:walk_app(App, ?CONFIG),
    io:format(" done~n"),
    Dict.

-spec process_module(module()) -> dict:dict().
process_module(Module) ->
    {_MFA, Dict} = kazoo_ast:walk_modules([Module], ?CONFIG),
    io:format(" done~n"),
    Dict.

%% @doc these excluded modules are not CRUD-y or deprecated
-spec start_module(module(), acc()) -> acc() | {'skip', acc()}.
start_module('cb_about', Acc) -> {'skip', Acc};
start_module('cb_access_lists', Acc) -> {'skip', Acc};
start_module('cb_acdc_call_stats', Acc) -> {'skip', Acc};
start_module('cb_acls', Acc) -> {'skip', Acc};
start_module('cb_agents', Acc) -> {'skip', Acc};
start_module('cb_alerts', Acc) -> {'skip', Acc};
start_module('cb_allotments', Acc) -> {'skip', Acc};
start_module('cb_api_auth', Acc) -> {'skip', Acc};
start_module('cb_apps_link', Acc) -> {'skip', Acc};
start_module('cb_apps_store', Acc) -> {'skip', Acc};
start_module('cb_att_handlers_errors', Acc) -> {'skip', Acc};
start_module('cb_auth', Acc) -> {'skip', Acc};
start_module('cb_basic_auth', Acc) -> {'skip', Acc};
start_module('cb_braintree', Acc) -> {'skip', Acc};
start_module('cb_call_inspector', Acc) -> {'skip', Acc};
start_module('cb_configs', Acc) -> {'skip', Acc};
start_module('cb_contact_list', Acc) -> {'skip', Acc};
start_module('cb_ip_auth', Acc) -> {'skip', Acc};
start_module('cb_migrations', Acc) -> {'skip', Acc};
start_module('cb_multi_factor', Acc) -> {'skip', Acc};
start_module('cb_presence', Acc) -> {'skip', Acc};
start_module('cb_quickcall', Acc) -> {'skip', Acc};
start_module('cb_security', Acc) -> {'skip', Acc};
start_module('cb_sup', Acc) -> {'skip', Acc};
start_module('cb_system_status', Acc) -> {'skip', Acc};
start_module('cb_token_auth', Acc) -> {'skip', Acc};
start_module('cb_token_restrictions', Acc) -> {'skip', Acc};
start_module('cb_user_auth', Acc) -> {'skip', Acc};
start_module('cb_websockets', Acc) -> {'skip', Acc};
start_module(Module, {_MFA, Dict}) ->
    {{Module, 'undefined', 0}, Dict}.

-spec print_dot(module(), acc()) -> acc().
print_dot(_M, Acc) ->
    io:format("."),
    Acc.

%% only process clauses from these two functions
-spec handle_allowed_methods(atom(), arity(), acc()) -> acc() | {'skip', acc()}.
handle_allowed_methods('allowed_methods'=F, 0=A, {{M, _F, _A}, Dict}) ->
    MFA = {M, F, A},
    {MFA, dict:store(MFA, [], Dict)};
handle_allowed_methods('allowed_methods'=F, 1=A, {{M, _F, _A}, Dict}) ->
    MFA = {M, F, A},
    {MFA, dict:store(MFA, [], Dict)};
handle_allowed_methods(_F, _A, Acc) -> {'skip', Acc}.

-spec handle_allowed_arity(atom(), list(), any(), acc()) -> acc() | {'skip', acc()}.
handle_allowed_arity('allowed_methods', [], _, {{_M, 'allowed_methods', 0}, _D}=Acc) -> Acc;
handle_allowed_arity('allowed_methods', [?VAR(_Var)], _, {{_M, 'allowed_methods', 1}, _D}=Acc) -> Acc;
handle_allowed_arity(_F, _Args, _Guards, Acc) -> {'skip', Acc}.

handle_verbs(?VAR(_), Acc) -> Acc;
handle_verbs(?EMPTY_LIST, Acc) -> Acc;
handle_verbs(?BINARY_STRING(Value), {MFA, Dict}) ->
    {MFA, dict:append(MFA, kz_term:to_binary(Value), Dict)};
handle_verbs(?BINARY_MATCH(_)=ASTBin, {MFA, Dict}) ->
    Bin = kz_ast_util:binary_match_to_binary(ASTBin),
    {MFA, dict:append(MFA, Bin, Dict)};
handle_verbs(_Expr, Acc) ->
    Acc.
