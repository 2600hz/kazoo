%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_file).

-export([get_uri/2]).
-export([maybe_prepare_proxy/1]).

-include("whistle_media.hrl").

-spec get_uri(ne_binaries() | ne_binary(), wh_json:object()) ->
                     {'ok', ne_binary()} |
                     {'error', 'not_found'} |
                     {'error', 'no_data'} |
                     {'error', 'no_stream_strategy'}.
get_uri(Media, JObj) when is_binary(Media) ->
    wh_util:put_callid(JObj),
    Paths = [Path
             || Path <- binary:split(Media, <<"/">>, ['global', 'trim']),
                (not wh_util:is_empty(Path))
            ],
    get_uri(Paths, JObj);
get_uri(Paths, JObj) ->
    case find_attachment(Paths) of
        {'error', _}=E -> E;
        {'ok', {Db, Id, Attachment}} ->
            maybe_local_haproxy_uri(JObj, Db, Id, Attachment)
    end.

-spec maybe_prepare_proxy(ne_binary()) -> 'ok' | 'error'.
maybe_prepare_proxy(URI) ->
    case wh_util:to_binary(props:get_value('path', uri_parser:parse(wh_util:to_list(URI), []), <<>>)) of
        <<"/single/", Rest/binary>> -> prepare_proxy(binary:split(Rest, <<"/">>, ['global', 'trim']));
        <<"/continuous/", Rest/binary>> -> prepare_proxy(binary:split(Rest, <<"/">>, ['global', 'trim']));
        _Else -> 'ok'
    end.

-spec prepare_proxy(ne_binaries()) -> 'ok' | 'error'.
prepare_proxy([Db, Id, Attachment]) ->
    case wh_media_cache_sup:find_file_server(Db, Id, Attachment) =:= {'error', 'no_file_server'} of
        'true' -> start_media_file_cache(Db, Id, Attachment);
        'false' -> lager:debug("existing file server for ~s/~s/~s", [Db, Id, Attachment])
    end.

-spec start_media_file_cache(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
start_media_file_cache(Db, Id, Attachment) ->
    case wh_media_cache_sup:start_file_server(Db, Id, Attachment) of
        {'ok', _FileServer} ->
            lager:debug("file server at ~p for ~s/~s/~s", [_FileServer, Db, Id, Attachment]);
        E ->
            lager:debug("failed to start server for ~s/~s/~s: ~p", [Db, Id, Attachment, E]),
            throw(E)
    end.

-spec find_attachment(ne_binaries() | ne_binary()) ->
                             {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                             {'error', 'not_found'}.
find_attachment([Id]) ->
    find_attachment([?WH_MEDIA_DB, Id]);
find_attachment([Db, Id]) ->
    find_attachment([Db, Id, 'first']);
find_attachment([Db, Id, 'first']) ->
    maybe_find_attachment(Db, Id);
find_attachment([Db = ?MEDIA_DB, Id, Attachment]) ->
    {'ok', {Db, Id, Attachment}};
find_attachment([Db, Id, Attachment]) ->
    AccountDb =  wh_util:format_account_id(Db, 'encoded'),
    {'ok', {AccountDb, Id, Attachment}};
find_attachment(Id) when not is_list(Id) ->
    find_attachment([Id]).

-spec maybe_find_attachment(ne_binary(), ne_binary()) ->
                                   {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                   {'error', 'not_found' | 'no_data'}.
-spec maybe_find_attachment(ne_binary(), ne_binary(), wh_json:object()) ->
                                   {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                   {'error', 'not_found' | 'no_data'}.
maybe_find_attachment(?MEDIA_DB = Db, Id) ->
    maybe_find_attachment_in_db(Db, Id);
maybe_find_attachment(Db, Id) ->
    AccountDb = wh_util:format_account_id(Db, 'encoded'),
    maybe_find_attachment_in_db(AccountDb, Id).

-spec maybe_find_attachment_in_db(ne_binary(), ne_binary()) ->
                                         {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                         {'error', 'not_found' | 'no_data'}.
maybe_find_attachment_in_db(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, Id, [{'cache_failures', ['not_found']}]) of
        {'error', _R} ->
            lager:debug("unable to open media doc ~s in ~s: ~p", [Id, Db, _R]),
            {'error', 'not_found'};
        {'ok', JObj} ->
            maybe_find_attachment(Db, Id, JObj)
    end.

maybe_find_attachment(Db, Id, JObj) ->
    lager:debug("trying to find first attachment on doc ~s in db ~s", [Id, Db]),
    case wh_doc:attachment_names(JObj) of
        [] ->
            lager:debug("media doc ~s in ~s has no attachments", [Id, Db]),
            {'error', 'no_data'};
        [AttachmentName | _] ->
            lager:debug("found first attachment ~s on ~s in ~s", [AttachmentName, Id, Db]),
            {'ok', {Db, cow_qs:urlencode(Id), AttachmentName}}
    end.

-spec maybe_local_haproxy_uri(wh_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                                     {'ok', ne_binary()} |
                                     {'error', 'no_stream_strategy'}.
maybe_local_haproxy_uri(JObj, Db, Id, Attachment) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_bigcouch_direct">>, 'true') of
        'false' -> maybe_media_manager_proxy_uri(JObj, Db, Id, Attachment);
        'true' ->
            Url = kz_datamgr:attachment_url(Db, Id, Attachment),
            {'ok', Url}
    end.

-spec maybe_media_manager_proxy_uri(wh_json:object(), ne_binary(), ne_binary(), ne_binary()) ->
                                           {'ok', ne_binary()} |
                                           {'error', 'no_stream_strategy'}.
maybe_media_manager_proxy_uri(JObj, Db, Id, Attachment) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_media_proxy">>, 'true') of
        'false' ->
            lager:warning("unable to build URL for media ~s ~s ~s", [Db, Id, Attachment]),
            {'error', 'no_stream_strategy'};
        'true' ->
            lager:debug("using media manager as proxy"),
            Host = wh_network_utils:get_hostname(),
            Port = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
            StreamType = wh_media_util:convert_stream_type(wh_json:get_value(<<"Stream-Type">>, JObj)),
            Permissions = case StreamType =:= <<"store">> of
                              'true' -> 'proxy_store';
                              'false' -> 'proxy_playback'
                          end,
            {'ok', <<(wh_media_util:base_url(Host, Port, Permissions))/binary
                     ,StreamType/binary
                     ,"/", Db/binary
                     ,"/", Id/binary
                     ,"/", Attachment/binary
                   >>
            }
    end.
