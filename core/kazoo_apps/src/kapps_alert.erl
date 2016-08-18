%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2016, 2600Hz inc
%%% @doc
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(kapps_alert).

-export([enabled/0
        ,fetch/1
        ,create/4, create/5
        ,save/1, delete/1
        ]).

-define(CONFIG_CAT, <<"alerts">>).

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_databases.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enabled() -> boolean().
enabled() ->
    kapps_config:get_is_true(?CONFIG_CAT, <<"enabled">>, 'true').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec fetch(ne_binary()) ->
                   {'ok', kz_json:object()} |
                   {'error', any()}.
fetch(AlertId) ->
    kzd_alert:fetch(AlertId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create(ne_binary(), ne_binary(), kz_json:object(), kz_json:object()) ->
                    {'ok', kzd_alert:doc()} |
                    {'error', any()}.
-spec create(ne_binary(), ne_binary(), kz_json:object()
            ,kz_json:object(), kz_proplist()
            ) ->
                    {'ok', kz_json:object()} |
                    {'required', ne_binary()} |
                    {'error', 'disabled'}.
create(Title, Message, From, To) ->
    create(Title, Message, From, To, []).

create('undefined', _Message, _From, _To, _Opts) ->
    {'required', kzd_alert:title()};
create(_Title, 'undefined', _From, _To, _Opts) ->
    {'required', kzd_alert:message()};
create(_Title, _Message, 'undefined', _To, _Opts) ->
    {'required', kzd_alert:from()};
create(_Title, _Message, _From, 'undefined', _Opts) ->
    {'required', kzd_alert:to()};
create(Title, Message, From, To, Opts) ->
    case enabled() of
        'false' -> {'error', 'disabled'};
        'true' ->
            Routines = [fun(J) -> kzd_alert:set_title(J, Title) end
                       ,fun(J) -> kzd_alert:set_message(J, Message) end
                       ,fun(J) -> kzd_alert:set_from(J, From) end
                       ,fun(J) -> kzd_alert:set_to(J, To) end
                       ,fun(J) -> maybe_add_options(J, Opts) end
                       ],
            {'ok', lists:foldl(fun(F, J) -> F(J) end, kzd_alert:new(), Routines)}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(kzd_alert:doc()) ->
                  {'ok', kzd_alert:doc()} |
                  {'error', any()}.
save(JObj) ->
    case enabled() of
        'false' -> {'error', 'alerts_disabled'};
        'true' ->
            kz_datamgr:save_doc(?KZ_ALERTS_DB, JObj)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(kzd_alert:doc() | ne_binary()) ->
                    {'ok', kzd_alert:doc()} |
                    {'error', any()}.
delete(AlertId) when is_binary(AlertId) ->
    case fetch(AlertId) of
        {'error', _}=Error -> Error;
        {'ok', JObj} -> delete(JObj)
    end;
delete(JObj) ->
    kz_datamgr:save_doc(?KZ_ALERTS_DB
                       ,kz_doc:set_soft_deleted(JObj, 'true')
                       ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_options(kz_json:object(), kz_proplist()) -> kz_json:object().
maybe_add_options(JObj, Props) ->
    Options = [{kzd_alert:category(), fun kzd_alert:set_category/2}
              ,{kzd_alert:metadata(), fun kzd_alert:set_metadata/2}
              ,{kzd_alert:level(), fun kzd_alert:set_level/2}
              ,{kzd_alert:expiration_date(), fun kzd_alert:set_expiration_date/2}
              ],
    lists:foldl(fun(Option, Acc) -> maybe_add_option(Option, Acc, Props) end
               ,JObj
               ,Options
               ).

-type update_fun() :: fun((kzd_alert:doc(), kz_json:json_term()) -> kzd_alert:doc()).
-spec maybe_add_option({ne_binary(), update_fun()}, kzd_alert:doc(), kz_proplist()) ->
                              kzd_alert:doc().
maybe_add_option({Option, Fun}, Acc, Props) ->
    case props:get_value(Option, Props) of
        'undefined' -> Acc;
        Value -> Fun(Acc, Value)
    end.
