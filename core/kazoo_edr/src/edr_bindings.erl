%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc EDR event distribution bindings
%%% @author Conversant Ltd (Max Lay)
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_bindings).

-include("edr.hrl").

%% Commonly used functions
-export([bind/3, bind/4
        ,bindings_from_json/1
        ,bindings_to_json/1
        ]).

%% You probably don't want to be using these unless you know what you're doing
-export([binding_keys/1
        ,event_binding_key/1
        ,distribute/1
        ]).

%% kazoo_bindings callbacks
-export([push/2]).

-record(binding_payload, {module     :: atom()
                         ,function   :: atom()
                         ,binding    :: edr_binding()
                         ,payload    :: any()
                         }).
-type binding_payload() :: #binding_payload{}.

-type bind_resp() :: ['ok' | {'error', 'exists'}].

-spec bind(edr_binding() | edr_bindings(), module(), atom()) -> bind_resp().
bind(Bindings, Module, Fun) ->
    bind(Bindings, Module, Fun, 'undefined').

-spec bind(edr_binding() | edr_bindings(), module(), atom(), any()) -> bind_resp().
bind(Bindings, Module, Fun, Payload) when is_list(Bindings) ->
    [bind(Binding, Module, Fun, Payload) || Binding <- Bindings];
bind(Binding, Module, Fun, Payload) ->
    kazoo_bindings:bind(binding_keys(Binding), ?MODULE, 'push', #binding_payload{module=Module
                                                                                ,function=Fun
                                                                                ,binding=Binding
                                                                                ,payload=Payload
                                                                                }).

-spec binding_keys(edr_binding()) -> kz_term:ne_binaries().
%% Accounts
binding_keys(#edr_binding{include_descendants='true'}=Binding) ->
    binding_keys(Binding#edr_binding{account_id= <<"*">>, include_descendants='false'});
binding_keys(#edr_binding{account_id=AccountId}=Binding) when not is_list(AccountId) ->
    binding_keys(Binding#edr_binding{account_id=[AccountId]});
%% Severity
binding_keys(#edr_binding{exact_severity='false', severity=Severity}=Binding) when not is_list(Severity) ->
    binding_keys(Binding#edr_binding{exact_severity='true', severity=levels(Severity, ?EDR_SEVERITY_LEVELS)});
binding_keys(#edr_binding{severity=Severity}=Binding) when not is_list(Severity) ->
    binding_keys(Binding#edr_binding{severity=[Severity]});
%% Verbosity
binding_keys(#edr_binding{exact_verbosity='false', verbosity=Verbosity}=Binding) when not is_list(Verbosity) ->
    binding_keys(Binding#edr_binding{exact_verbosity='true', verbosity=levels(Verbosity, ?EDR_VERBOSITY_LEVELS)});
binding_keys(#edr_binding{verbosity=Verbosity}=Binding) when not is_list(Verbosity) ->
    binding_keys(Binding#edr_binding{verbosity=[Verbosity]});
%% App name
binding_keys(#edr_binding{app_name=AppName}=Binding) when not is_list(AppName) ->
    binding_keys(Binding#edr_binding{app_name=[AppName]});
%% Actually get the bindings
binding_keys(#edr_binding{severity=Severities
                         ,verbosity=Verbosities
                         ,account_id=AccountIds
                         ,app_name=AppNames
                         }) ->
    [binding_key(S, V, Acc, App) || S <- Severities, V <- Verbosities, Acc <- AccountIds, App <- AppNames].

%% Levels equal to or more significant than the specified level
-spec levels(edr_severity(), edr_severities()) -> edr_severities();
            (edr_verbosity(), edr_verbosities()) -> edr_verbosities().
levels(Level, AllLevels) ->
    lists:dropwhile(fun(L) -> L =/= Level end, AllLevels).

-spec binding_key(edr_severity(), edr_verbosity(), kz_term:api_ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
binding_key(Severity, Verbosity, AccountId, AppName) ->
    list_to_binary(["edr."
                   ,kz_term:to_binary(Severity), "."
                   ,kz_term:to_binary(Verbosity), "."
                   ,kz_term:to_binary(AccountId), "."
                   ,kz_term:to_binary(AppName)
                   ]).

-spec event_binding_key(edr_event()) -> kz_term:ne_binary().
event_binding_key(#edr_event{account_id=AccountId
                            ,severity=Severity
                            ,verbosity=Verbosity
                            ,app_name=AppName
                            }) ->
    binding_key(Severity, Verbosity, AccountId, AppName).

-spec distribute(edr_event()) -> 'ok'.
distribute(#edr_event{}=Event) ->
    kazoo_bindings:map(event_binding_key(Event), Event),
    'ok'.

-spec push(binding_payload(), edr_event()) -> any().
push(#binding_payload{module=Module
                     ,function=Fun
                     ,binding=Binding
                     ,payload=Payload
                     }
    ,#edr_event{}=Event) ->
    case should_push(Binding, Event) of
        'true' when Payload =:= 'undefined' -> erlang:apply(Module, Fun, [Event]);
        'true' -> erlang:apply(Module, Fun, [Payload, Event]);
        'false' -> 'ok'
    end.

%% Handle more complex bindings that can't be solved with routing keys here
-spec should_push(edr_binding(), edr_event()) -> boolean().
should_push(#edr_binding{account_id=AccountId, include_descendants=true}
           ,#edr_event{account_tree=AccountTree}) ->
    AccountTree =/= 'undefined'
        andalso lists:member(AccountId, AccountTree);
should_push(_Binding, _Event) ->
    'true'.

-spec bindings_from_json(kz_json:object()) -> edr_binding();
                        (kz_json:objects()) -> edr_bindings().
bindings_from_json(JObjs) when is_list(JObjs) ->
    [bindings_from_json(JObj) || JObj <- JObjs];
bindings_from_json(JObj) ->
    #edr_binding{account_id=kz_doc:account_id(JObj, <<"*">>)
                ,include_descendants=kz_json:is_true(<<"include_descendants">>, JObj, 'false')
                ,app_name=kz_json:get_binary_value(<<"app_name">>, JObj, <<"*">>)
                ,severity=severity_from_json(kz_json:get_value(<<"severity">>, JObj))
                ,exact_severity=kz_json:is_true(<<"exact_severity">>, JObj, 'false')
                ,verbosity=verbosity_from_json(kz_json:get_value(<<"verbosity">>, JObj))
                ,exact_verbosity=kz_json:is_true(<<"exact_verbosity">>, JObj, 'false')
                }.

-spec severity_from_json(kz_term:api_ne_binary() | kz_term:api_ne_binaries()) -> edr_severity().
severity_from_json(Severities) when is_list(Severities) ->
    [severity_from_json(Severity) || Severity <- Severities];
severity_from_json(Severity) when is_binary(Severity) ->
    'true' = lists:member(Severity, ?EDR_SEVERITY_BINARIES),
    kz_term:to_atom(Severity);
severity_from_json('undefined') ->
    'info'.

-spec verbosity_from_json(kz_term:api_ne_binary() | kz_term:api_ne_binaries()) -> edr_verbosity().
verbosity_from_json(Verbosities) when is_list(Verbosities) ->
    [verbosity_from_json(Verbosity) || Verbosity <- Verbosities];
verbosity_from_json(Verbosity) when is_binary(Verbosity) ->
    'true' = lists:member(Verbosity, ?EDR_VERBOSITY_BINARIES),
    kz_term:to_atom(Verbosity);
verbosity_from_json('undefined') ->
    'ok'.

-spec bindings_to_json(edr_binding()) -> kz_json:object();
                      (edr_bindings()) -> kz_json:objects().
bindings_to_json(Bindings) when is_list(Bindings) ->
    [bindings_to_json(Binding) || Binding <- Bindings];
bindings_to_json(Binding=#edr_binding{}) ->
    kz_json:from_list([{<<"account_id">>, Binding#edr_binding.account_id}
                      ,{<<"include_descendants">>, Binding#edr_binding.include_descendants}
                      ,{<<"app_name">>, Binding#edr_binding.app_name}
                      ,{<<"severity">>, severity_or_verbosity_to_json(Binding#edr_binding.severity)}
                      ,{<<"exact_severity">>, Binding#edr_binding.exact_severity}
                      ,{<<"verbosity">>, severity_or_verbosity_to_json(Binding#edr_binding.verbosity)}
                      ,{<<"exact_verbosity">>, Binding#edr_binding.exact_verbosity}
                      ]).

-spec severity_or_verbosity_to_json(edr_severity() | edr_verbosity()) -> kz_term:ne_binary();
                                   (edr_severities() | edr_verbosities()) -> kz_term:ne_binaries().
severity_or_verbosity_to_json(Values) when is_list(Values) ->
    [severity_or_verbosity_to_json(Value) || Value <- Values];
severity_or_verbosity_to_json(Value) ->
    kz_term:to_binary(Value).
