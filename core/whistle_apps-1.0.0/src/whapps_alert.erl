%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz inc
%%% @doc
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(whapps_alert).

-export([
    fetch/1
    ,create/4, create/5
    ,save/1, delete/1
]).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) -> {'ok', wh_json:object()} |
                            {'error', any()}.
fetch(AlertId) ->
    kzd_alert:fetch(AlertId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), ne_binary(), wh_json:object(), wh_json:object()) -> {'ok', wh_json:object()} |
                                                                              {'error', any()}.
-spec create(ne_binary(), ne_binary(), wh_json:object()
             ,wh_json:object(), wh_proplist()) -> {'ok', wh_json:object()} |
                                                  {'error', any()}.
create(Title, Message, From, To) ->
    create(Title, Message, From, To, []).

create('undefined', _Message, _From, _To, _Opts) ->
    {'error', 'missing_title'};
create(_Title, 'undefined', _From, _To, _Opts) ->
    {'error', 'missing_message'};
create(_Title, _Message, 'undefined', _To, _Opts) ->
    {'error', 'missing_from'};
create(_Title, _Message, _From, 'undefined', _Opts) ->
    {'error', 'missing_to'};
create(Title, Message, From, To, Opts) ->
    JObj = maybe_add_options(Opts, kzd_alert:new()),
    Routines = [
        fun(J) -> kzd_alert:set_title(J, Title) end
        ,fun(J) -> kzd_alert:set_message(J, Message) end
        ,fun(J) -> kzd_alert:set_from(J, From) end
        ,fun(J) -> kzd_alert:set_to(J, To) end
    ],
    {'ok', lists:foldl(fun(F, J) -> F(J) end, JObj, Routines)}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(wh_json:object()) -> {'ok', wh_json:object()} | {'error', any()}.
save(JObj) ->
    couch_mgr:save_doc(?WH_ALERTS_DB, JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(wh_json:object()) -> {'ok', wh_json:object()} | {'error', any()}.
delete(AlertId) when is_binary(AlertId) ->
    case ?MODULE:fetch(AlertId) of
        {'error', _}=Error -> Error;
        {'ok', JObj} -> delete(JObj)
    end;
delete(JObj) ->
    couch_mgr:save_doc(?WH_ALERTS_DB, JObj).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_options(wh_proplist(), wh_json:object()) -> wh_json:object().
maybe_add_options(Props, JObj) ->
    Options = [
        {kzd_alert:category(), fun kzd_alert:category/2}
        ,{kzd_alert:metadata(), fun kzd_alert:metadata/2}
        ,{kzd_alert:level(), fun kzd_alert:level/2}
        ,{kzd_alert:expiration_date(), fun kzd_alert:expiration_date/2}
    ],
    lists:foldl(
        fun({Option, Fun}, Acc) ->
            case props:get_value(Option, Props) of
                'undefined' -> Acc;
                Value ->
                    Fun(JObj, Value)
            end
        end
        ,JObj
        ,Options
    ).