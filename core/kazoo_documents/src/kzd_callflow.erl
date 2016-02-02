%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2015, 2600Hz
%%% @doc
%%% Device document manipulation
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzd_callflow).

-export([new/0
         ,type/0
         ,is_feature_code/1
        ]).

-include("kz_documents.hrl").

-type doc() :: wh_json:object().
-export_type([doc/0]).

-define(FEATURE_CODE, <<"featurecode">>).
-define(PVT_TYPE, <<"callflow">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    wh_json:from_list([{<<"pvt_type">>, type()}]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_feature_code(wh_json:object()) -> boolean().
is_feature_code(JObj) ->
    case wh_json:get_value(?FEATURE_CODE, JObj, 'false') of
        'false' -> 'false';
        _ -> 'true'
    end.

