%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson <karl@2600hz.org>
%%% @end
%%%-----------------------------------------------------------------------------
-module(notify_fax_util).

-export([get_attachment/1, get_attachment/2]).

-include("notify.hrl").

-define(CONVERT_CONFIG_CAT, <<"kazoo_convert">>).

%%------------------------------------------------------------------------------
%% @doc create a friendly file name
%% @end
%%------------------------------------------------------------------------------
-spec get_file_name(kz_term:proplist(), kz_term:ne_binary()) -> kz_term:ne_binary().
get_file_name(Props, Ext) ->
    Fax = props:get_value(<<"fax">>, Props),
    CallerID = case {props:get_value(<<"caller_id_name">>, Fax), props:get_value(<<"caller_id_number">>, Fax)} of
                   {'undefined', 'undefined'} -> <<"Unknown">>;
                   {'undefined', Num} -> kz_term:to_binary(Num);
                   {Name, _} -> kz_term:to_binary(Name)
               end,
    LocalDateTime = props:get_value(<<"date_called">>, Fax, <<"0000-00-00_00-00-00">>),
    FName = list_to_binary([CallerID, "_", kz_time:pretty_print_datetime(LocalDateTime), ".", Ext]),
    re:replace(kz_term:to_lower_binary(FName), <<"\\s+">>, <<"_">>, [{'return', 'binary'}, 'global']).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_attachment(kz_term:proplist()) ->
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()} |
                            {'error', any()}.
get_attachment(Props) ->
    UseDb = props:get_value(<<"account_db">>, Props, ?KZ_FAXES_DB),
    get_attachment(UseDb, Props).

-spec get_attachment(kz_term:ne_binary(), kz_term:proplist()) ->
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()} |
                            {'error', any()}.
get_attachment(UseDb, Props) ->
    Fax   = props:get_value(<<"fax">>, Props),
    FaxId = props:get_first_defined([<<"fax_jobid">>, <<"fax_id">>], Fax),
    {'ok', Content, ContentType} = raw_attachment_binary(UseDb, FaxId),
    {ContentType, get_file_name(Props, kz_mime:to_extension(ContentType)), Content}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec raw_attachment_binary(kz_term:ne_binary(), kz_term:ne_binary()) ->
                                   {'ok', kz_term:ne_binary(), kz_term:ne_binary()}.
raw_attachment_binary(Db, FaxId) ->
    raw_attachment_binary(Db, FaxId, 2).

-spec raw_attachment_binary(kz_term:ne_binary(), kz_term:ne_binary(), non_neg_integer()) ->
                                   {'ok', kz_term:ne_binary(), kz_term:ne_binary()}.
raw_attachment_binary(Db, FaxId, Retries) when Retries > 0 ->
    lager:debug("get raw attachment ~s / ~s", [Db, FaxId]),

    case kz_datamgr:open_doc(Db, FaxId) of
        {'error','not_found'} when Db =/= ?KZ_FAXES_DB ->
            raw_attachment_binary(?KZ_FAXES_DB, FaxId, Retries);
        {'ok', FaxJObj} ->
            Format = kapps_config:get_ne_binary(?CONVERT_CONFIG_CAT, [<<"fax">>, <<"attachment_format">>], <<"pdf">>),
            case kz_fax_attachment:fetch(Format, Db, FaxJObj) of
                {'ok', Content, ContentType, _Doc} ->
                    {'ok', Content, ContentType};
                {'error', Error} ->
                    lager:debug("failed to find the attachment with error ~p, retrying ~b more times", [Error, Retries]),
                    timer:sleep(?MILLISECONDS_IN_MINUTE * 5),
                    raw_attachment_binary(Db, FaxId, Retries)
            end
    end.
