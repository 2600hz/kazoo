%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2022, 2600Hz
%%% @doc Kazoo API Definition Helpers.
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_definition).

%% #kapi_definition record accessors
-export([name/1
        ,friendly_name/1
        ,description/1
        ,build_fun/1
        ,validate_fun/1
        ,publish_fun/1
        ,binding/1
        ,restrict_to/1
        ,required_headers/1
        ,optional_headers/1
        ,values/1
        ,types/1
        ]).

-include_lib("kz_amqp_util.hrl").

-type api() :: #kapi_definition{}.
-type apis() :: [api()].

-export_type([api/0, apis/0]).

-spec name(api()) -> kz_term:ne_binary().
name(#kapi_definition{name = Name}) -> Name.

-spec friendly_name(api()) -> kz_term:ne_binary().
friendly_name(#kapi_definition{friendly_name = FreindlyName}) -> FreindlyName.

-spec description(api()) -> kz_term:ne_binary().
description(#kapi_definition{description = Description}) -> Description.

-spec build_fun(api()) -> fun((kz_term:api_terms()) -> api_formatter_return()).
build_fun(#kapi_definition{build_fun = BuildFun}) -> BuildFun.

-spec validate_fun(api()) -> fun((kz_term:api_terms()) -> boolean()).
validate_fun(#kapi_definition{validate_fun = ValidateFun}) -> ValidateFun.

-spec publish_fun(api()) -> fun((kz_term:api_terms()) -> api_formatter_return()).
publish_fun(#kapi_definition{publish_fun = PublishFun}) -> PublishFun.

-spec binding(api()) -> kz_term:api_ne_binary().
binding(#kapi_definition{binding = Binding}) -> Binding.

-spec restrict_to(api()) -> kz_term:api_atom().
restrict_to(#kapi_definition{restrict_to = RestrictTo}) -> RestrictTo.

-spec required_headers(api()) -> api_headers().
required_headers(#kapi_definition{required_headers = ReqH}) -> ReqH.

-spec optional_headers(api()) -> api_headers().
optional_headers(#kapi_definition{optional_headers = OptH}) -> OptH.

-spec values(api()) -> api_valid_values().
values(#kapi_definition{values = Values}) -> Values.

-spec types(api()) -> api_types().
types(#kapi_definition{types = Types}) -> Types.
