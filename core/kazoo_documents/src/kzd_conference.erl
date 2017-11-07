-module(kzd_conference).

-export([new/0
        ,type/0
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(PVT_TYPE, <<"conference">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json:set_value(<<"pvt_type">>, type(), kz_json_schema:default_object(<<"conferences">>)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec type() -> ne_binary().
type() -> ?PVT_TYPE.
