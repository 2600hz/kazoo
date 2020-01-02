%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Account module
%%% @author Jon Blanton <jon@2600hz.com>
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_whitelabel).

-export([init/0
        ,allowed_methods/0, allowed_methods/1, allowed_methods/2
        ,resource_exists/0, resource_exists/1, resource_exists/2
        ,authenticate/2, authenticate/3
        ,authorize/2, authorize/3
        ,validate/1, validate/2, validate/3
        ,content_types_provided/2, content_types_provided/3
        ,content_types_accepted/2
        ,put/1
        ,post/1, post/2
        ,delete/1

        ,acceptable_content_types/0
        ]).

-include("crossbar.hrl").

-define(WHITELABEL_ID, <<"whitelabel">>).
-define(LOGO_REQ, <<"logo">>).
-define(ICON_REQ, <<"icon">>).
-define(DOMAINS_REQ, <<"domains">>).
-define(WELCOME_REQ, <<"welcome">>).

-define(WHITELABEL_MIME_TYPES, ?IMAGE_CONTENT_TYPES ++ ?BASE64_CONTENT_TYPES).

%% Commonly found ico mime types
-define(WHITELABEL_ICON_MIME_TYPES, [{<<"image">>, <<"ico">>, '*'}
                                    ,{<<"image">>, <<"vnd.microsoft.icon">>, '*'}
                                    ,{<<"image">>, <<"x-icon">>, '*'}
                                    ,{<<"image">>, <<"icon">>, '*'}
                                     | ?WHITELABEL_MIME_TYPES
                                    ]).

-define(WHITELABEL_WELCOME_MIME_TYPES, [{<<"text">>, <<"html">>, '*'}]).

-define(AGG_VIEW_WHITELABEL_DOMAIN, <<"accounts/list_by_whitelabel_domain">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.authenticate.whitelabel">>, 'authenticate'}
               ,{<<"*.authorize.whitelabel">>, 'authorize'}
               ,{<<"*.content_types_provided.whitelabel">>, 'content_types_provided'}
               ,{<<"*.content_types_accepted.whitelabel">>, 'content_types_accepted'}
               ,{<<"*.allowed_methods.whitelabel">>, 'allowed_methods'}
               ,{<<"*.resource_exists.whitelabel">>, 'resource_exists'}
               ,{<<"*.validate.whitelabel">>, 'validate'}
               ,{<<"*.execute.put.whitelabel">>, 'put'}
               ,{<<"*.execute.post.whitelabel">>, 'post'}
               ,{<<"*.execute.delete.whitelabel">>, 'delete'}
               ],
    _ = cb_modules_util:bind(?MODULE, Bindings),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(?LOGO_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?ICON_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?WELCOME_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(?DOMAINS_REQ) ->
    [?HTTP_GET, ?HTTP_POST];
allowed_methods(_WhitelabelDomain) ->
    [?HTTP_GET].

-spec allowed_methods(path_token(), path_token()) -> http_methods().
allowed_methods(_WhitelabelDomain, ?LOGO_REQ) ->
    [?HTTP_GET];
allowed_methods(_WhitelabelDomain, ?ICON_REQ) ->
    [?HTTP_GET];
allowed_methods(_WhitelabelDomain, ?WELCOME_REQ) ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(?LOGO_REQ) -> 'true';
resource_exists(?ICON_REQ) -> 'true';
resource_exists(?WELCOME_REQ) -> 'true';
resource_exists(?DOMAINS_REQ) -> 'true';
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), path_token()) -> 'true'.
resource_exists(_, ?LOGO_REQ) -> 'true';
resource_exists(_, ?WELCOME_REQ) -> 'true';
resource_exists(_, ?ICON_REQ) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context(), path_token()) -> boolean().
authorize(Context, ?DOMAINS_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET
        orelse cb_context:is_superduper_admin(Context);
authorize(Context, _Domain) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET.

-spec authorize(cb_context:context(), path_token(), path_token()) -> boolean().
authorize(Context, _Domain, ?LOGO_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET;
authorize(Context, _Domain, ?ICON_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET;
authorize(Context, _Domain, ?WELCOME_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(Context, _Domain) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET.

-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().
authenticate(Context, _Domain, ?LOGO_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET;
authenticate(Context, _Domain, ?ICON_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET;
authenticate(Context, _Domain, ?WELCOME_REQ) ->
    cb_context:req_verb(Context) =:= ?HTTP_GET.

%%------------------------------------------------------------------------------
%% @doc Add content types accepted and provided by this module
%% @end
%%------------------------------------------------------------------------------

-spec acceptable_content_types() -> cowboy_content_types().
acceptable_content_types() ->
    ?WHITELABEL_WELCOME_MIME_TYPES ++ ?WHITELABEL_ICON_MIME_TYPES.

-spec content_types_provided(cb_context:context(), path_token()) ->
          cb_context:context().
content_types_provided(Context, AttachType) ->
    content_types_provided_for_attachments(Context, AttachType, cb_context:req_verb(Context)).

-spec content_types_provided_for_attachments(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
content_types_provided_for_attachments(Context, ?LOGO_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?LOGO_REQ);
content_types_provided_for_attachments(Context, ?ICON_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?ICON_REQ);
content_types_provided_for_attachments(Context, ?WELCOME_REQ, ?HTTP_GET) ->
    content_types_provided_for_attachments(Context, ?WELCOME_REQ);
content_types_provided_for_attachments(Context, _Type, _Verb) ->
    Context.

-spec content_types_provided_for_attachments(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
content_types_provided_for_attachments(Context, AttachType) ->
    Context1 = load_whitelabel_meta(Context, ?WHITELABEL_ID),
    case whitelabel_binary_meta(Context1, AttachType) of
        'undefined' -> Context1;
        {_, JObj} -> set_content_type_provided(Context1, JObj)
    end.

-spec content_types_provided(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided(Context, Domain, AttachType) ->
    content_types_provided_for_domain_attachments(Context, Domain, AttachType, cb_context:req_verb(Context)).

-spec content_types_provided_for_domain_attachments(cb_context:context(), path_token(), path_token(), http_method()) ->
          cb_context:context().
content_types_provided_for_domain_attachments(Context, Domain, ?LOGO_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?LOGO_REQ);
content_types_provided_for_domain_attachments(Context, Domain, ?ICON_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?ICON_REQ);
content_types_provided_for_domain_attachments(Context, Domain, ?WELCOME_REQ, ?HTTP_GET) ->
    content_types_provided_for_domain_attachments(Context, Domain, ?WELCOME_REQ);
content_types_provided_for_domain_attachments(Context, _Domain, _AttachType, _Verb) ->
    Context.

-spec content_types_provided_for_domain_attachments(cb_context:context(), path_token(), path_token()) ->
          cb_context:context().
content_types_provided_for_domain_attachments(Context, Domain, AttachType) ->
    case find_whitelabel_binary_meta(Context, Domain, AttachType) of
        'undefined' -> Context;
        {_, JObj} -> set_content_type_provided(Context, JObj)
    end.

-spec set_content_type_provided(cb_context:context(), kz_json:object()) -> cb_context:context().
set_content_type_provided(Context, JObj) ->
    CT = kz_json:get_value(<<"content_type">>, JObj),
    [Type, SubType] = binary:split(CT, <<"/">>),
    cb_context:set_content_types_provided(Context, [{'to_binary', [{Type, SubType, []}]}]).

-spec content_types_accepted(cb_context:context(), path_token()) -> cb_context:context().
content_types_accepted(Context, AttachType) ->
    content_types_accepted(Context, AttachType, cb_context:req_verb(Context)).

-spec content_types_accepted(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
content_types_accepted(Context, ?LOGO_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, ?ICON_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_ICON_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, ?WELCOME_REQ, ?HTTP_POST) ->
    CTA = [{'from_binary', ?WHITELABEL_WELCOME_MIME_TYPES}],
    cb_context:set_content_types_accepted(Context, CTA);
content_types_accepted(Context, _AttachType, _Verb) ->
    Context.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    validate_whitelabel(Context, cb_context:req_verb(Context)).

-spec validate_whitelabel(cb_context:context(), http_method()) ->
          cb_context:context().
validate_whitelabel(Context, ?HTTP_GET) ->
    load_whitelabel_meta(Context, ?WHITELABEL_ID);
validate_whitelabel(Context, ?HTTP_PUT) ->
    validate_request(Context, 'undefined');
validate_whitelabel(Context, ?HTTP_POST) ->
    validate_request(Context, ?WHITELABEL_ID);
validate_whitelabel(Context, ?HTTP_DELETE) ->
    load_whitelabel_meta(Context, ?WHITELABEL_ID).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?LOGO_REQ) ->
    validate_attachment(Context, ?LOGO_REQ, cb_context:req_verb(Context));
validate(Context, ?ICON_REQ) ->
    validate_attachment(Context, ?ICON_REQ, cb_context:req_verb(Context));
validate(Context, ?WELCOME_REQ) ->
    validate_attachment(Context, ?WELCOME_REQ, cb_context:req_verb(Context));
validate(Context, ?DOMAINS_REQ) ->
    validate_domains(Context, cb_context:req_verb(Context));
validate(Context, Domain) ->
    validate_domain(Context, Domain, cb_context:req_verb(Context)).

-spec validate_attachment(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
validate_attachment(Context, ?LOGO_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?LOGO_REQ);
validate_attachment(Context, ?ICON_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?ICON_REQ);
validate_attachment(Context, ?WELCOME_REQ, ?HTTP_GET) ->
    load_whitelabel_binary(Context, ?WELCOME_REQ);
validate_attachment(Context, AttachType, ?HTTP_POST) ->
    validate_attachment_post(Context, AttachType, cb_context:req_files(Context)).

-spec validate_attachment_post(cb_context:context(), path_token(), any()) ->
          cb_context:context().
validate_attachment_post(Context, ?LOGO_REQ, []) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide an image file">>}])
                                   ,Context
                                   );
validate_attachment_post(Context, ?ICON_REQ, []) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide an image file">>}])
                                   ,Context
                                   );
validate_attachment_post(Context, ?WELCOME_REQ, []) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide an html file">>}])
                                   ,Context
                                   );
validate_attachment_post(Context, ?LOGO_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?ICON_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?WELCOME_REQ, [{_Filename, FileJObj}]) ->
    validate_upload(Context, FileJObj);
validate_attachment_post(Context, ?LOGO_REQ, _Files) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide a single image file">>}])
                                   ,Context
                                   );
validate_attachment_post(Context, ?ICON_REQ, _Files) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"Please provide a single image file">>}])
                                   ,Context
                                   );
validate_attachment_post(Context, ?WELCOME_REQ, _Files) ->
    cb_context:add_validation_error(<<"file">>
                                   ,<<"maxItems">>
                                   ,kz_json:from_list([{<<"message">>, <<"please provide a single html file">>}])
                                   ,Context
                                   ).

-spec validate_upload(cb_context:context(), kz_json:object()) ->
          cb_context:context().
validate_upload(Context, FileJObj) ->
    Context1 = load_whitelabel_meta(Context, ?WHITELABEL_ID),
    case cb_context:resp_status(Context) of
        'success' ->
            Props = [{<<"content_type">>, content_type(FileJObj)}
                    ,{<<"content_length">>, file_size(FileJObj)}
                    ],
            validate_request(cb_context:set_req_data(Context1
                                                    ,kz_json:set_values(Props, cb_context:doc(Context))
                                                    )
                            ,?WHITELABEL_ID
                            );
        _Status -> Context1
    end.

-spec content_type(kz_json:object()) -> kz_term:ne_binary().
content_type(FileJObj) ->
    kz_json:get_value([<<"headers">>, <<"content_type">>]
                     ,FileJObj
                     ,<<"application/octet-stream">>
                     ).

-spec file_size(kz_json:object()) -> non_neg_integer().
file_size(FileJObj) ->
    case kz_json:get_integer_value([<<"headers">>, <<"content_length">>], FileJObj) of
        'undefined' ->
            byte_size(kz_json:get_value(<<"contents">>, FileJObj, <<>>));
        Size -> Size
    end.

-spec validate_domains(cb_context:context(), http_method()) ->
          cb_context:context().
validate_domains(Context, ?HTTP_GET) ->
    load_domains(Context);
validate_domains(Context, ?HTTP_POST) ->
    case cb_context:account_id(Context) of
        'undefined' -> edit_domains(Context);
        _AccountId -> test_account_domains(Context)
    end.

-spec load_domains(cb_context:context()) ->
          cb_context:context().
load_domains(Context) ->
    load_domains(Context, find_domain(Context)).

-spec load_domains(cb_context:context(), kz_term:api_binary()) ->
          cb_context:context().
load_domains(Context, 'undefined') ->
    missing_domain_error(Context);
load_domains(Context, Domain) ->
    load_domains(Context, Domain, system_domains()).

-spec load_domains(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
load_domains(Context, Domain, SystemDomains) ->
    AccountDomains = kzd_domains:format(SystemDomains, Domain),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, AccountDomains}
                       ,{fun cb_context:set_resp_status/2, 'success'}
                       ]).

-spec missing_domain_error(cb_context:context()) -> cb_context:context().
missing_domain_error(Context) ->
    cb_context:add_validation_error(<<"domain">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"No domain found to use. Supply one on the request or create one via the whitelabel API">>}])
                                   ,Context
                                   ).

-spec find_domain(cb_context:context()) -> kz_term:api_binary().
find_domain(Context) ->
    case cb_context:req_value(Context, <<"domain">>) of
        'undefined' -> find_existing_domain(Context);
        Domain -> Domain
    end.

-spec find_existing_domain(cb_context:context()) -> kz_term:api_binary().
find_existing_domain(Context) ->
    Context1 = load_whitelabel_meta(Context, ?WHITELABEL_ID),
    case cb_context:resp_status(Context1) of
        'success' -> kz_json:get_value(<<"domain">>, cb_context:doc(Context1));
        _Status -> 'undefined'
    end.

-spec system_domains() -> kz_json:object().
system_domains() ->
    case kapps_config:get_json(<<"whitelabel">>, <<"domains">>) of
        'undefined' ->
            lager:info("initializing system domains to default"),
            Default = kzd_domains:default(),
            _ = kapps_config:set_default(<<"whitelabel">>, <<"domains">>, Default),
            Default;
        Domains -> Domains
    end.

-spec edit_domains(cb_context:context()) -> cb_context:context().
edit_domains(Context) ->
    Domains = cb_context:req_data(Context),
    PvtFields = crossbar_doc:update_pvt_parameters(kz_json:new()
                                                  ,Context
                                                  ),

    lager:debug("saving domains ~p", [Domains]),
    lager:debug("with pvt fields: ~p", [PvtFields]),

    case kzd_domains:save(Domains, PvtFields) of
        {'error', 'not_found'} ->
            lager:debug("schema for domains is missing"),
            missing_schema_error(Context);
        {'error', Errors} ->
            lager:debug("failed to save with ~p", [Errors]),
            cb_context:failed(Context, Errors);
        {'ok', SavedDomains} ->
            lager:debug("saved domains: ~p", [SavedDomains]),
            crossbar_util:response(SavedDomains, Context)
    end.

-spec missing_schema_error(cb_context:context()) ->
          cb_context:context().
missing_schema_error(Context) ->
    cb_context:add_validation_error(<<"domains">>
                                   ,<<"required">>
                                   ,kz_json:from_list([{<<"message">>, <<"The domains schema is missing, unable to validate request">>}])
                                   ,Context
                                   ).

-spec test_account_domains(cb_context:context()) ->
          cb_context:context().
test_account_domains(Context) ->
    Context1 = load_domains(Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            test_account_domains(Context, cb_context:resp_data(Context1));
        _Status ->
            Context1
    end.

-spec test_account_domains(cb_context:context(), kzd_domains:doc()) ->
          cb_context:context().
test_account_domains(Context, DomainsJObj) ->
    Options = test_network_options(Context),
    TestResults =
        kz_json:map(fun(DomainType, DomainConfig) ->
                            test_domains(DomainType, DomainConfig, Options)
                    end
                   ,DomainsJObj
                   ),
    crossbar_util:response(TestResults, Context).

-spec test_domains(kz_term:ne_binary(), kz_json:object(), kz_network_utils:options()) ->
          {kz_term:ne_binary(), kz_json:object()}.
test_domains(DomainType, DomainConfig, Options) ->
    {DomainType, test_domain_config(DomainType, DomainConfig, Options)}.

-spec test_domain_config(kz_term:ne_binary(), kz_json:object(), kz_network_utils:options()) ->
          kz_json:object().
test_domain_config(DomainType, DomainConfig, Options) ->
    kz_json:map(fun(Host, HostConfig) ->
                        {Host, test_host(Host, HostConfig, DomainType, Options)}
                end
               ,DomainConfig
               ).

-spec test_host(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_network_utils:options()) ->
          kz_json:object().
test_host(Host, HostConfig, DomainType, Options) ->
    kz_json:from_list([{<<"expected">>, kzd_domains:mappings(HostConfig)}
                      ,{<<"actual">>, lookup(Host, DomainType, Options)}
                      ,{<<"name">>, kzd_domains:name(HostConfig)}
                      ]).

-spec lookup(kz_term:ne_binary(), kz_term:ne_binary(), kz_network_utils:options()) ->
          kz_term:ne_binaries().
lookup(Host, DomainType, Options) ->
    Type = kz_term:to_atom(kz_term:to_lower_binary(DomainType)),
    {'ok', Lookup} = kz_network_utils:lookup_dns(Host, Type, Options),
    lager:debug("lookup of ~s(~s): ~p", [Host, Type, Lookup]),
    format_lookup_results(Type, Lookup).

-spec format_lookup_results(atom(), [inet_res:dns_data()]) ->
          kz_term:ne_binaries().
format_lookup_results(Type, Lookup) ->
    [format_lookup_result(Type, Result) || Result <- Lookup].

-spec format_lookup_result(atom(), inet_res:dns_data()) -> kz_term:ne_binary().
format_lookup_result('naptr', {_, _, _, _, _, _}=NAPTR) ->
    kz_network_utils:naptrtuple_to_binary(NAPTR);
format_lookup_result('srv', {_, _, _, _}=Srv) ->
    kz_network_utils:srvtuple_to_binary(Srv);
format_lookup_result('mx', {_, _}=MX) ->
    kz_network_utils:mxtuple_to_binary(MX);
format_lookup_result(_, {_, _, _, _}=IP) ->
    kz_network_utils:iptuple_to_binary(IP);
format_lookup_result(_, {_, _, _, _, _, _, _, _}=IP) ->
    kz_network_utils:iptuple_to_binary(IP);
format_lookup_result(_Type, Value) ->
    lager:debug("attempt to convert ~s result to binary: ~p", [Value]),
    kz_term:to_binary(Value).

-spec test_network_options(cb_context:context()) -> kz_network_utils:options().
test_network_options(Context) ->
    Domain = find_domain(Context),
    DefaultOptions = kz_network_utils:default_options(),
    case kz_network_utils:find_nameservers(Domain) of
        [] ->  DefaultOptions;
        Nameservers ->
            kz_network_utils:set_option_nameservers(DefaultOptions, Nameservers)
    end.

-spec validate_domain(cb_context:context(), path_token(), http_method()) ->
          cb_context:context().
validate_domain(Context, Domain, ?HTTP_GET) ->
    case cb_context:account_id(Context) of
        'undefined' -> find_whitelabel_meta(Context, Domain);
        _AccountId -> load_whitelabel_meta(Context, ?WHITELABEL_ID)
    end.

-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context, Domain, AttachType) ->
    validate_domain_attachment(Context, Domain, AttachType, cb_context:req_verb(Context)).

validate_domain_attachment(Context, Domain, AttachType, ?HTTP_GET) ->
    find_whitelabel_binary(Context, Domain, AttachType).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

-spec post(cb_context:context()) -> cb_context:context().
post(Context) ->
    maybe_update_account_definition(crossbar_doc:save(Context)).

-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, ?LOGO_REQ) ->
    update_whitelabel_binary(?LOGO_REQ, ?WHITELABEL_ID, Context);
post(Context, ?ICON_REQ) ->
    update_whitelabel_binary(?ICON_REQ, ?WHITELABEL_ID, Context);
post(Context, ?WELCOME_REQ) ->
    update_whitelabel_binary(?WELCOME_REQ, ?WHITELABEL_ID, Context);
post(Context, ?DOMAINS_REQ) ->
    cb_context:set_resp_status(Context, 'success').

-spec delete(cb_context:context()) -> cb_context:context().
delete(Context) ->
    maybe_cleanup_account_definition(crossbar_doc:delete(Context, ?HARD_DELETE)).

%%------------------------------------------------------------------------------
%% @doc Load the binary attachment of a whitelabel doc (based on a domain)
%% @end
%%------------------------------------------------------------------------------
-spec find_whitelabel(cb_context:context(), kz_term:ne_binary()) ->
          cb_context:context().
find_whitelabel(Context, Domain) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Domain)}],
    Context1 = crossbar_doc:load_view(?AGG_VIEW_WHITELABEL_DOMAIN
                                     ,ViewOptions
                                     ,cb_context:set_db_name(Context, ?KZ_ACCOUNTS_DB)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' ->
            case cb_context:doc(Context1) of
                [JObj] ->
                    Db = kz_json:get_ne_value([<<"value">>, <<"account_db">>], JObj),
                    Id = kz_json:get_ne_value([<<"value">>, <<"account_id">>], JObj),
                    cb_context:setters(Context1
                                      ,[{fun cb_context:set_db_name/2, Db}
                                       ,{fun cb_context:set_account_id/2, Id}
                                       ]);
                _Doc ->
                    cb_context:add_system_error('bad_identifier'
                                               ,kz_json:from_list([{<<"cause">>, Domain}])
                                               ,Context1
                                               )
            end;
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Load a whitelabel document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_whitelabel_meta(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_whitelabel_meta(Context, WhitelabelId) ->
    crossbar_doc:load(WhitelabelId, Context, ?TYPE_CHECK_OPTION(<<"whitelabel">>)).

-spec find_whitelabel_meta(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
find_whitelabel_meta(Context, Domain) ->
    Context1 = find_whitelabel(Context, Domain),
    case cb_context:resp_status(Context1) of
        'success' -> load_whitelabel_meta(Context1, ?WHITELABEL_ID);
        _Status -> Context1
    end.

%%------------------------------------------------------------------------------
%% @doc Load a whitelabel binary attachment from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_whitelabel_binary(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_whitelabel_binary(Context, AttachType) ->
    case whitelabel_binary_meta(Context, AttachType) of
        'undefined' -> crossbar_util:response_bad_identifier(AttachType, Context);
        {AttachmentId, JObj} ->
            update_response_with_attachment(Context, AttachmentId, JObj)
    end.

-spec find_whitelabel_binary(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
find_whitelabel_binary(Context, Domain, AttachType) ->
    Context1 = find_whitelabel_meta(Context, Domain),
    case kz_doc:id(cb_context:doc(Context)) =:= ?WHITELABEL_ID
        orelse cb_context:resp_status(Context1) =:= 'success'
    of
        'true' -> load_whitelabel_binary(Context1, AttachType);
        'false' -> Context1
    end.

-spec find_whitelabel_binary_meta(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          'undefined' | {kz_term:ne_binary(), kz_json:object()}.
find_whitelabel_binary_meta(Context, Domain, AttachType) ->
    Context1 = find_whitelabel_meta(Context, Domain),
    case kz_doc:id(cb_context:doc(Context)) =:= ?WHITELABEL_ID
        orelse cb_context:resp_status(Context1) =:= 'success'
    of
        'true' -> whitelabel_binary_meta(Context1, AttachType);
        'false' -> 'undefined'
    end.

-spec whitelabel_binary_meta(cb_context:context(), kz_term:ne_binary()) ->
          'undefined' | {kz_term:ne_binary(), kz_json:object()}.
whitelabel_binary_meta(Context, AttachType) ->
    whitelabel_binary_meta(Context, AttachType, cb_context:doc(Context)).

whitelabel_binary_meta(Context, AttachType, JObj) ->
    case kz_doc:id(JObj) =:= ?WHITELABEL_ID
        orelse cb_context:resp_status(Context) =:= 'success'
    of
        'true' ->
            Attachments = kz_doc:attachments(JObj, kz_json:new()),
            case whitelabel_attachment_id(Attachments, AttachType) of
                'undefined' -> 'undefined';
                AttachmentId ->
                    {AttachmentId, kz_json:get_json_value(AttachmentId, Attachments)}
            end;
        'false' -> 'undefined'
    end.

-spec whitelabel_attachment_id(kz_json:object(), kz_term:ne_binary()) ->
          kz_term:api_ne_binary().
whitelabel_attachment_id(Attachments, AttachType) ->
    AttachmentIds = kz_json:get_keys(Attachments),
    filter_attachment_type(AttachmentIds, AttachType).

-spec filter_attachment_type(kz_term:ne_binaries(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
filter_attachment_type([], _) -> 'undefined';

%% if there is only one attachment and it is not an icon
%%  then it is the deprecated whitelabel when we assumed
%%  the only attachment was the logo...
filter_attachment_type([<<"icon", _/binary>>], <<"logo">>) -> 'undefined';
filter_attachment_type([AttachmentId], <<"logo">>) -> AttachmentId;

filter_attachment_type([AttachmentId|AttachmentIds], AttachType) ->
    case binary:match(AttachmentId, AttachType) of
        {0, _} -> AttachmentId;
        _Else -> filter_attachment_type(AttachmentIds, AttachType)
    end.

-spec update_response_with_attachment(cb_context:context(), kz_term:ne_binary(), kz_json:object()) ->
          cb_context:context().
update_response_with_attachment(Context, AttachmentId, JObj) ->
    LoadedContext = crossbar_doc:load_attachment(cb_context:doc(Context), AttachmentId, ?TYPE_CHECK_OPTION(<<"whitelabel">>), Context),
    WithHeaders = cb_context:add_resp_headers(LoadedContext

                                             ,#{<<"content-disposition">> => <<"attachment; filename=", AttachmentId/binary>>
                                               ,<<"content-type">> => kz_json:get_value([AttachmentId, <<"content_type">>], JObj)
                                               }
                                             ),
    cb_context:set_resp_etag(WithHeaders, 'undefined').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_request(Context, WhitelabelId) ->
    validate_unique_domain(Context, WhitelabelId).

-spec validate_unique_domain(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
validate_unique_domain(Context, WhitelabelId) ->
    Domain = kz_json:get_ne_value(<<"domain">>, cb_context:req_data(Context)),
    case is_domain_unique(cb_context:account_id(Context), Domain) of
        'true' -> check_whitelabel_schema(Context, WhitelabelId);
        'false' ->
            Context1 =
                cb_context:add_validation_error(<<"domain">>
                                               ,<<"unique">>
                                               ,kz_json:from_list(
                                                  [{<<"message">>, <<"White label domain is already in use">>}
                                                  ,{<<"cause">>, Domain}
                                                  ])
                                               ,Context
                                               ),
            check_whitelabel_schema(Context1, WhitelabelId)
    end.

-spec check_whitelabel_schema(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
check_whitelabel_schema(Context, WhitelabelId) ->
    OnSuccess = fun(C) -> on_successful_validation(C, WhitelabelId) end,
    cb_context:validate_request_data(<<"whitelabel">>, Context, OnSuccess).

-spec on_successful_validation(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
on_successful_validation(Context, 'undefined') ->
    Doc = kz_json:set_values([{<<"pvt_type">>, <<"whitelabel">>}
                             ,{<<"_id">>, ?WHITELABEL_ID}
                             ], cb_context:doc(Context)),
    cb_context:set_doc(Context, Doc);
on_successful_validation(Context, WhitelabelId) ->
    crossbar_doc:load_merge(WhitelabelId, Context, ?TYPE_CHECK_OPTION(<<"whitelabel">>)).

%%------------------------------------------------------------------------------
%% @doc Update the binary attachment of a whitelabel doc
%% @end
%%------------------------------------------------------------------------------
-spec update_whitelabel_binary(kz_term:ne_binary(), path_token(), cb_context:context()) ->
          cb_context:context().
update_whitelabel_binary(AttachType, WhitelabelId, Context) ->
    JObj = cb_context:doc(Context),
    [{Filename, FileObj}] = cb_context:req_files(Context),
    Contents = kz_json:get_value(<<"contents">>, FileObj),
    CT = kz_json:get_value([<<"headers">>, <<"content_type">>], FileObj),
    Opts = [{'content_type', CT} | ?TYPE_CHECK_OPTION(<<"whitelabel">>)],

    JObj1 = case whitelabel_binary_meta(Context, AttachType) of
                'undefined' -> JObj;
                {AttachmentId, _} ->
                    kz_doc:delete_attachment(JObj, AttachmentId)
            end,
    Context1 = crossbar_doc:save(cb_context:set_doc(Context, JObj1)),

    crossbar_doc:save_attachment(WhitelabelId
                                ,attachment_name(AttachType, Filename, CT)
                                ,Contents
                                ,Context1
                                ,Opts
                                ).

%%------------------------------------------------------------------------------
%% @doc Generate an attachment name if one is not provided and ensure
%% it has an extension (for the associated content type)
%% @end
%%------------------------------------------------------------------------------

-spec attachment_name(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
attachment_name(Filename, CT) ->
    Generators = [fun maybe_create_basename/1
                 ,fun(A) -> maybe_add_extension(A, CT) end
                 ],
    lists:foldl(fun(F, A) -> F(A) end, Filename, Generators).

-spec maybe_create_basename(kz_term:api_binary()) -> kz_term:ne_binary().
maybe_create_basename(A) ->
    case kz_term:is_empty(A) of
        'true' -> kz_binary:rand_hex(16);
        'false' -> A
    end.

-spec maybe_add_extension(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
maybe_add_extension(A, CT) ->
    case kz_term:is_empty(filename:extension(A)) of
        'false' -> A;
        'true' ->
            <<A/binary, ".", (kz_mime:to_extension(CT))/binary>>
    end.

-spec attachment_name(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
attachment_name(AttachType, Filename, CT) ->
    <<AttachType/binary, "-", (attachment_name(Filename, CT))/binary>>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_domain_unique(kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
is_domain_unique(AccountId, Domain) ->
    ViewOptions = [{'key', kz_term:to_lower_binary(Domain)}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_WHITELABEL_DOMAIN, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} ->
            kz_json:get_ne_value([<<"value">>, <<"account_id">>], JObj) =:= AccountId;
        {'ok', _} -> 'false';
        {'error', _R} ->
            lager:debug("unable to get whitelabel domain view: ~p", [_R]),
            'false'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_account_definition(cb_context:context()) -> cb_context:context().
maybe_update_account_definition(Context) ->
    maybe_update_account_definition(Context, cb_context:resp_status(Context)).
maybe_update_account_definition(Context, 'success') ->
    Context1 = crossbar_doc:load(cb_context:account_id(Context), Context, ?TYPE_CHECK_OPTION(kzd_accounts:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            AccountDoc = cb_context:doc(Context1),
            Domain = kz_json:get_ne_value(<<"domain">>, cb_context:doc(Context)),
            AccountDoc1 = kz_json:set_value(<<"pvt_whitelabel_domain">>, Domain, AccountDoc),
            _ = cb_accounts:post(cb_context:set_doc(Context1, AccountDoc1), cb_context:account_id(Context)),
            Context;
        _Status -> Context1
    end;
maybe_update_account_definition(Context, _Status) -> Context.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_cleanup_account_definition(cb_context:context()) -> cb_context:context().
maybe_cleanup_account_definition(Context) ->
    maybe_cleanup_account_definition(Context, cb_context:resp_status(Context)).

maybe_cleanup_account_definition(Context, 'success') ->
    Context1 = crossbar_doc:load(cb_context:account_id(Context), Context, ?TYPE_CHECK_OPTION(kzd_accounts:type())),
    case cb_context:resp_status(Context1) of
        'success' ->
            AccountDoc = cb_context:doc(Context1),
            AccountDoc1 = kz_json:delete_key(<<"pvt_whitelabel_domain">>, AccountDoc),
            _ = cb_accounts:post(cb_context:set_doc(Context1, AccountDoc1), cb_context:account_id(Context)),
            Context;
        _Status -> Context1
    end;
maybe_cleanup_account_definition(Context, _Status) -> Context.
