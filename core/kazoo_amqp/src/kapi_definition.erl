%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017-2020, 2600Hz
%%% @doc Kazoo API Definition Helpers.
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_definition).

-export([build_message/2
        ,validate/2
        ,event_type_headers/2
        ,setters/1, setters/2
        ]).

%% #kapi_definition record accessors
-export([binding/1, set_binding/2
        ,build_fun/1, set_build_fun/2
        ,category/1, set_category/2
        ,description/1, set_description/2
        ,friendly_name/1, set_friendly_name/2
        ,name/1, set_name/2
        ,optional_headers/1, set_optional_headers/2
        ,publish_fun/1, set_publish_fun/2
        ,required_headers/1, set_required_headers/2
        ,restrict_to/1, set_restrict_to/2
        ,types/1, set_types/2
        ,validate_fun/1, set_validate_fun/2
        ,values/1, set_values/2
        ]).

-include_lib("kazoo_amqp/include/kapi_definition.hrl").
-include_lib("kazoo_amqp/include/kz_api_literals.hrl").

-type setter_fun() :: {fun((api(), Value) -> api()), Value}.
-type setter_funs() :: [setter_fun()].

-opaque api() :: #kapi_definition{}.
-type apis() :: [api()].

-export_type([api/0
             ,apis/0
             ,setter_funs/0
             ]).

%%%=============================================================================
%%% Accessors
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(api()) -> kz_term:ne_binary().
name(#kapi_definition{name = Name}) -> Name.

-spec set_name(api(), kz_term:ne_binary()) -> api().
set_name(API, Name) -> API#kapi_definition{name = Name}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category(api()) -> kz_term:ne_binary().
category(#kapi_definition{category = Category}) -> Category.

-spec set_category(api(), kz_term:ne_binary()) -> api().
set_category(API, Category) -> API#kapi_definition{category = Category}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec friendly_name(api()) -> kz_term:ne_binary().
friendly_name(#kapi_definition{friendly_name = FreindlyName}) -> FreindlyName.

-spec set_friendly_name(api(), kz_term:ne_binary()) -> api().
set_friendly_name(API, Name) -> API#kapi_definition{friendly_name = Name}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec description(api()) -> kz_term:ne_binary().
description(#kapi_definition{description = Description}) -> Description.

-spec set_description(api(), kz_term:ne_binary()) -> api().
set_description(API, Description) -> API#kapi_definition{description = Description}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_fun(api()) -> fun((kz_term:api_terms()) -> kz_api:api_formatter_return()).
build_fun(#kapi_definition{build_fun = BuildFun}) -> BuildFun.

-spec set_build_fun(api(), fun((kz_term:api_terms()) -> kz_api:api_formatter_return())) -> api().
set_build_fun(API, BuildFun) -> API#kapi_definition{build_fun = BuildFun}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_fun(api()) -> fun((kz_term:api_terms()) -> boolean()).
validate_fun(#kapi_definition{validate_fun = ValidateFun}) -> ValidateFun.

-spec set_validate_fun(api(), fun((kz_term:api_terms()) -> boolean())) -> api().
set_validate_fun(API, ValidateFun) -> API#kapi_definition{validate_fun = ValidateFun}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_fun(api()) -> fun((kz_term:api_terms()) -> kz_api:api_formatter_return()).
publish_fun(#kapi_definition{publish_fun = PublishFun}) -> PublishFun.

-spec set_publish_fun(api(), fun((kz_term:api_terms()) -> kz_api:api_formatter_return())) -> api().
set_publish_fun(API, PublishFun) -> API#kapi_definition{publish_fun = PublishFun}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec binding(api()) -> binding().
binding(#kapi_definition{binding = Binding}) -> Binding.

-spec set_binding(api(), binding()) -> api().
set_binding(API, Binding) -> API#kapi_definition{binding = Binding}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec restrict_to(api()) -> kz_term:api_atom().
restrict_to(#kapi_definition{restrict_to = RestrictTo}) -> RestrictTo.

-spec set_restrict_to(api(), kz_term:api_atom()) -> api().
set_restrict_to(API, RestrictTo) -> API#kapi_definition{restrict_to = RestrictTo}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec required_headers(api()) -> kz_api:api_headers().
required_headers(#kapi_definition{required_headers = ReqH}) -> ReqH.

-spec set_required_headers(api(), kz_api:api_headers()) -> api().
set_required_headers(API, ReqH) -> API#kapi_definition{required_headers = ReqH}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec optional_headers(api()) -> kz_api:api_headers().
optional_headers(#kapi_definition{optional_headers = OptH}) -> OptH.

-spec set_optional_headers(api(), kz_api:api_headers()) -> api().
set_optional_headers(API, OptH) -> API#kapi_definition{optional_headers = OptH}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec values(api()) -> kz_api:api_valid_values().
values(#kapi_definition{values = Values}) -> Values.

-spec set_values(api(), kz_api:api_valid_values()) -> api().
set_values(API, Values) -> API#kapi_definition{values = Values}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec types(api()) -> kz_api:api_types().
types(#kapi_definition{types = Types}) -> Types.

-spec set_types(api(), kz_api:api_types()) -> api().
set_types(API, Types) -> API#kapi_definition{types = Types}.

%%%=============================================================================
%%% Utilities
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> api().
setters(Routines) ->
    setters(Routines, #kapi_definition{}).

-spec setters(setter_funs(), api()) -> api().
setters(Routines, Definition) ->
    lists:foldl(fun({Setter, Value}, A) -> Setter(A, Value) end
               ,Definition
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec event_type_headers(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_term:proplist().
event_type_headers(Category, EventName) ->
    [{?KEY_EVENT_CATEGORY, Category}
    ,{?KEY_EVENT_NAME, EventName}
    ].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), api()) -> kz_api:api_formatter_return().
build_message(Prop, #kapi_definition{validate_fun = ValidateFun
                                    ,required_headers = ReqHeaders
                                    ,optional_headers = OptionalHeaders
                                    ,binding = Binding
                                    ,friendly_name = FriendlyName
                                    ,name = Name
                                    }) when is_list(Prop) ->
    case ValidateFun(Prop) of
        'true' ->
            kz_api:build_message(Prop, ReqHeaders, OptionalHeaders);
        'false' ->
            Id = hd(lists:dropwhile(fun kz_term:is_empty/1, [Binding, FriendlyName, Name])),
            {'error', "Proplist failed validation for " ++ kz_term:to_list(Id)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
-spec validate(kz_term:api_terms(), api()) -> boolean().
validate(Prop, #kapi_definition{required_headers = ReqH
                               ,values = Values
                               ,types = Types
                               }) when is_list(Prop) ->
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).
