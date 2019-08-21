%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_plan).

-export([id/1
        ,set_id/2
        ]).
-export([vendor_id/1
        ,set_vendor_id/2
        ]).
-export([bookkeeper_id/1]).
-export([bookkeeper_type/1]).
-export([bookkeeper_vendor_id/1]).
-export([default_bookkeeper/1
        ,set_default_bookkeeper/2
        ]).
-export([ratedeck_id/1]).
-export([ratedeck_name/1]).
-export([applications/1]).
-export([limits/1]).
-export([jobj/1
        ,set_jobj/2
        ]).
-export([plan_jobj/1
        ,set_plan_jobj/2
        ]).
-export([overrides/1
        ,set_overrides/2
        ]).
-export([bookkeeper_hash/1]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).
-export([is_plan/1]).

-export([fetch/2]).
-export([create/3]).
-export([assign/2
        ,assign/3
        ]).
-export([unassign/2]).
-export([override/3
        ,override/4
        ]).

-record(plan, {id :: kz_term:api_ne_binary()
              ,vendor_id :: kz_term:api_ne_binary()
              ,jobj = kzd_service_plan:new() :: kzd_service_plan:doc()
              ,plan_jobj = kzd_service_plan:new() :: kzd_service_plan:doc()
              ,default_bookkeeper = kz_json:new() :: kz_json:object()
              ,overrides = kz_json:new() :: kz_json:object()
              }
       ).

-opaque plan() :: #plan{}.
-type setter_fun() :: {fun((plan(), Value) -> plan()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([plan/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec id(plan()) -> kz_term:ne_binary().
id(#plan{id=Id}) ->
    Id.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_id(plan(), kz_term:ne_binary()) -> plan().
set_id(Plan, Id) ->
    Plan#plan{id=Id}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec vendor_id(plan()) -> kz_term:ne_binary().
vendor_id(#plan{vendor_id=VendorId}) ->
    VendorId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_vendor_id(plan(), kz_term:ne_binary()) -> plan().
set_vendor_id(Plan, VendorId) ->
    Plan#plan{vendor_id=VendorId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_id(plan()) -> kz_term:ne_binary().
bookkeeper_id(Plan) ->
    kzd_service_plan:bookkeeper_id(jobj(Plan), kzd_services:default_bookkeeper_id()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(plan()) -> kz_term:ne_binary().
bookkeeper_type(Plan) ->
    kzd_service_plan:bookkeeper_type(jobj(Plan), kzd_services:default_bookkeeper_type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(plan()) -> kz_term:ne_binary().
bookkeeper_vendor_id(Plan) ->
    kzd_service_plan:bookkeeper_vendor_id(jobj(Plan), vendor_id(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec default_bookkeeper(plan()) -> kz_json:object().
default_bookkeeper(#plan{default_bookkeeper=Bookkeeper}) ->
    Bookkeeper.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_default_bookkeeper(plan(), kz_json:object()) -> plan().
set_default_bookkeeper(Plan, Bookkeeper) ->
    JObj = kz_json:merge_recursive(
             [kz_json:from_list([{<<"bookkeeper">>, Bookkeeper}])
             ,jobj(Plan)
             ]
            ),
    set_jobj(Plan#plan{default_bookkeeper=Bookkeeper}, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ratedeck_id(plan()) -> kz_term:api_ne_binary().
ratedeck_id(Plan) ->
    kzd_service_plan:ratedeck_id(jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ratedeck_name(plan()) -> kz_term:api_ne_binary().
ratedeck_name(Plan) ->
    kzd_service_plan:ratedeck_name(jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec applications(plan()) -> kz_json:object().
applications(Plan) ->
    kzd_service_plan:applications(jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec limits(plan()) -> kz_json:object().
limits(Plan) ->
    kzd_service_plan:limits(jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec jobj(plan()) -> kz_json:object().
jobj(#plan{jobj=JObj}) ->
    JObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_jobj(plan(), kz_json:object()) -> plan().
set_jobj(Plan, JObj) ->
    Plan#plan{jobj=JObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_jobj(plan()) -> kzd_service_plan:doc().
plan_jobj(#plan{plan_jobj=PlanJObj}) ->
    PlanJObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan_jobj(plan(), kzd_service_plan:doc()) -> plan().
set_plan_jobj(Plan, PlanJObj) ->
    Routines = [{fun set_overrides/2, overrides(Plan)}
               ,{fun set_default_bookkeeper/2, default_bookkeeper(Plan)}
               ],
    setters(Plan#plan{plan_jobj=PlanJObj}, Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec overrides(plan()) -> kz_json:object().
overrides(#plan{overrides=Overrides}) ->
    Overrides.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_overrides(plan(), kz_term:api_object()) -> plan().
set_overrides(Plan, Overrides) ->
    case kz_term:is_empty(Overrides) of
        'true' -> set_jobj(Plan, plan_jobj(Plan));
        'false' ->
            JObj = kz_json:merge_recursive(plan_jobj(Plan), Overrides),
            set_jobj(Plan, JObj)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_hash(plan()) -> kz_term:ne_binary().
bookkeeper_hash(Plan) ->
    kz_binary:md5(
      <<(kz_term:to_binary(bookkeeper_id(Plan)))/binary
       ,(kz_term:to_binary(bookkeeper_type(Plan)))/binary
       ,(kz_term:to_binary(bookkeeper_vendor_id(Plan)))/binary
      >>
     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> plan().
empty() ->
    #plan{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> plan().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(plan(), setter_funs()) -> plan().
setters(Plan, Routines) ->
    lists:foldl(fun({Setter, Value}, P) ->
                        Setter(P, Value)
                end
               ,Plan
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(plan()) -> kz_json:object().
public_json(Plan) ->
    kz_doc:public_fields(jobj(Plan)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_plan(plan()) -> boolean().
is_plan(#plan{}) -> 'true';
is_plan(_Else) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary(), kz_term:ne_binary()) -> 'undefined' | plan().
fetch(VendorId, PlanId) ->
    lager:debug("fetching plan ~s/~s", [VendorId, PlanId]),
    VendorDb = kz_util:format_account_db(VendorId),
    case kz_datamgr:open_cache_doc(VendorDb, PlanId) of
        {'ok', PlanJObj} ->
            create(VendorId, PlanId, PlanJObj);
        {'error', _R} ->
            lager:info("unable to open service plan ~s/~s: ~p"
                      ,[VendorId, PlanId, _R]
                      ),
            'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_term:ne_binary(), kz_term:ne_binary(), kzd_service_plan:doc()) -> plan().
create(VendorId, PlanId, PJObj) ->
    PlanJObj = maybe_ensure_bookkeeper_vendor(PJObj, VendorId),
    Setters = [{fun set_id/2, PlanId}
              ,{fun set_vendor_id/2, VendorId}
              ,{fun set_jobj/2, PlanJObj}
              ,{fun set_plan_jobj/2, PlanJObj}
              ],
    setters(Setters).

-spec maybe_ensure_bookkeeper_vendor(kzd_service_plan:doc(), kz_term:ne_binary()) -> kzd_service_plan:plan().
maybe_ensure_bookkeeper_vendor(PlanJObj, VendorId) ->
    case kz_term:is_not_empty(kzd_service_plan:bookkeeper(PlanJObj))
        andalso kz_term:is_empty(kzd_service_plan:bookkeeper_vendor_id(PlanJObj))
    of
        'true' -> kzd_service_plan:set_bookkeeper_vendor_id(PlanJObj, VendorId);
        'false' -> PlanJObj
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
assign(Services, PlanId) ->
    assign(Services, PlanId, []).

-spec assign(kz_services:services(), kz_term:ne_binary() | kz_json:object(), kz_term:proplist()) -> kz_services:services().
assign(Services, PlanId, Options) when is_binary(PlanId) ->
    ResellerId = kz_services_reseller:get_id(Services),
    Plan = kz_json:from_list([{<<"vendor_id">>, ResellerId}]),
    assign(Services, PlanId, Plan, Options);
assign(Services, JObj, Options) ->
    ResellerId = kz_services_reseller:get_id(Services),
    VendorId = kz_json:get_ne_binary_value(<<"vendor_id">>, JObj, ResellerId),
    Overrides = kz_json:get_json_value(<<"overrides">>, JObj),
    Contract = kz_json:get_json_value(<<"contract">>, JObj),

    case kz_doc:id(JObj) of
        'undefined' -> Services;
        PlanId ->
            Plan = kz_json:from_list(
                     [{<<"contract">>, Contract}
                     ,{<<"vendor_id">>, VendorId}
                     ,{<<"overrides">>, Overrides}
                     ]
                    ),
            assign(Services, PlanId, Plan, Options)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assign(kz_services:services(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_services:services().
assign(Services, PlanId, Plan, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    lager:debug("adding service plan ~s/~s"
               ,[kzd_services:plan_vendor_id(ServicesJObj, PlanId)
                ,PlanId
                ]
               ),
    Overrides = kz_json:get_json_value(<<"overrides">>, Plan),
    Overriden = maybe_override(ServicesJObj, PlanId, Overrides, Options),
    UpdatedServicesJObj =
        kzd_services:set_plan(ServicesJObj
                             ,PlanId
                             ,kz_json:set_value(<<"overrides">>, Overriden, Plan)
                             ),
    kz_services:set_services_jobj(Services, UpdatedServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec unassign(kz_services:services(), kz_term:ne_binary()) -> kz_services:services().
unassign(Services, PlanId) ->
    lager:debug("removing service plan ~s", [PlanId]),
    ServicesJObj = kz_services:services_jobj(Services),
    kz_services:set_services_jobj(Services
                                 ,kzd_services:set_plan(ServicesJObj, PlanId, 'undefined')
                                 ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_term:ne_binary(), kz_json:object()) -> kz_services:services().
override(Services, PlanId, Overrides) ->
    override(Services, PlanId, Overrides, []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_services:services().
override(Services, PlanId, Overrides, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    Overriden = maybe_override(ServicesJObj, PlanId, Overrides, Options),
    kz_services:set_services_jobj(Services
                                 ,kzd_services:set_plan_overrides(ServicesJObj, PlanId, Overriden)
                                 ).

-spec maybe_override(kz_json:object(), kz_term:ne_binary(), kz_term:api_object(), kz_term:proplist()) -> kz_json:object().
maybe_override(ServicesJObj, PlanId, 'undefined', _Options) ->
    kzd_services:plan_overrides(ServicesJObj, PlanId);
maybe_override(ServicesJObj, PlanId, Overrides, Options) ->
    case kz_json:is_empty(kzd_services:plan_overrides(ServicesJObj, PlanId)) of
        'false' -> set_or_merge_override(ServicesJObj, PlanId, Overrides, Options);
        'true' -> Overrides
    end.

-spec set_or_merge_override(kz_json:object(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> kz_json:object().
set_or_merge_override(ServicesJObj, PlanId, Override, Options) ->
    case props:get_is_true('merge', Options, 'false') of
        'true' -> merge_override(ServicesJObj, PlanId, Override);
        'false' -> set_override(ServicesJObj, PlanId, Override)
    end.

-spec set_override(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
set_override(_ServicesJObj, PlanId, Overrides)  ->
    lager:debug("updating overrides for ~s via set", [PlanId]),
    Overrides.

-spec merge_override(kz_json:object(), kz_term:ne_binary(), kz_json:object()) -> kz_json:object().
merge_override(ServicesJObj, PlanId, Override) ->
    lager:debug("updating overrides for ~s via merge", [PlanId]),
    Overrides = kzd_services:plan_overrides(ServicesJObj, PlanId, kz_json:new()),
    kz_json:merge_recursive(Overrides, Override).
