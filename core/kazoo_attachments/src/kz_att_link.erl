%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% DB Link for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_link).

-include("kz_att.hrl").

-define(REQUIRED_PROPS, [<<"att_dbname">>
                        ,<<"att_docid">>
                        ]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([put_attachment/6]).
-export([fetch_attachment/4]).

-spec put_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_data:options()) -> any().
put_attachment(_Params, _DbName, _DocId, _AName, _Contents, Options) ->
    Props = props:filter_undefined(
              [{<<"att_dbname">>, pros:get_value('att_dbname', Options)}
              ,{<<"att_docid">>, pros:get_value('att_name', Options)}
              ,{<<"att_name">>, pros:get_value('att_name', Options)}
              ]),
    case ?REQUIRED_PROPS -- props:get_keys(Props) of
        [] -> {'ok', [{'attachment', Props}]};
        Missing -> {'error', <<"missing required : ", (kz_util:join_binary(Missing, <<",">>))/binary>>}
    end.


-spec fetch_attachment(kz_data:connection(), ne_binary(), ne_binary(), ne_binary()) -> any().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"att_name">>, HandlerProps) of
        'undefined' -> get_att_name(HandlerProps);
        AttName -> fetch_attachment(HandlerProps, AttName)
    end.

get_att_name(HandlerProps) ->
    DbName = kz_json:get_value(<<"att_dbname">>, HandlerProps),
    DocId = kz_json:get_value(<<"att_docid">>, HandlerProps),
    case kz_datamgr:open_cache_doc(DbName, DocId) of
        {'ok', JObj} -> maybe_fetch_attachment(HandlerProps, kz_doc:attachment_names(JObj));
        _ -> {'error', 'not_found'}
    end.

maybe_fetch_attachment(_HandlerProps, []) -> {'error', 'not_found'};
maybe_fetch_attachment(HandlerProps, [AName | _]) ->
    fetch_attachment(HandlerProps, AName).

fetch_attachment(HandlerProps, AName) ->
    DbName = kz_json:get_value(<<"att_dbname">>, HandlerProps),
    DocId = kz_json:get_value(<<"att_docid">>, HandlerProps),
    kz_datamgr:fetch_attachment(DbName, DocId, AName).
