%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% DB Link for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_link).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(REQUIRED_PROPS, [<<"att_dbname">>
                        ,<<"att_docid">>
                        ]).

%% ====================================================================
%% `gen_attachment' behaviour callbacks (API)
%% ====================================================================
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(_Params, _DbName, _DocId, _AName, _Contents, Options) ->
    Props = props:filter_undefined(
              [{<<"att_dbname">>, props:get_value('att_dbname', Options)}
              ,{<<"att_docid">>, props:get_value('att_docid', Options)}
              ,{<<"att_name">>, props:get_value('att_name', Options)}
              ]),
    case ?REQUIRED_PROPS -- props:get_keys(Props) of
        [] ->
            {'ok', [{'attachment', Props}]};
        Missing ->
            Error = <<"missing required : ", (kz_binary:join(Missing, <<", ">>))/binary>>,
            gen_attachment:error_response(400, Error)
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"att_name">>, HandlerProps) of
        'undefined' -> get_att_name(HandlerProps);
        AttName -> fetch_attachment(HandlerProps, AttName)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
get_att_name(HandlerProps) ->
    DbName = kz_json:get_value(<<"att_dbname">>, HandlerProps),
    DocId = kz_json:get_value(<<"att_docid">>, HandlerProps),
    case kz_datamgr:open_cache_doc(DbName, DocId) of
        {'ok', JObj} -> maybe_fetch_attachment(HandlerProps, kz_doc:attachment_names(JObj));
        _ -> gen_attachment:error_response(404, 'not_found')
    end.

maybe_fetch_attachment(_HandlerProps, []) -> gen_attachment:error_response(404, 'not_found');
maybe_fetch_attachment(HandlerProps, [AName | _]) ->
    fetch_attachment(HandlerProps, AName).

fetch_attachment(HandlerProps, AName) ->
    DbName = kz_json:get_value(<<"att_dbname">>, HandlerProps),
    DocId = kz_json:get_value(<<"att_docid">>, HandlerProps),
    case kz_datamgr:fetch_attachment(DbName, DocId, AName) of
        {'ok', _Body} = Resp ->
            Resp;
        {'error', Reason} = _E when is_atom(Reason) ->
            lager:debug("Error fetching attachment: ~p", [_E]),
            gen_attachment:error_response(400, Reason);
        {'error', Reason} = _E when is_integer(Reason) ->
            lager:debug("Error fetching attachment: ~p", [_E]),
            gen_attachment:error_response(Reason, <<"Error fetching the attachment.">>);
        {'error', {Code, Body}} = _E when is_integer(Code) ->
            lager:debug("Error fetching attachment: ~p", [_E]),
            gen_attachment:error_response(Code, Body);
        {'error', Reason} = _E when is_tuple(Reason) ->
            lager:debug("Error fetching attachment: ~p", [_E]),
            gen_attachment:error_response(400, kz_binary:join(tuple_to_list(Reason)))
    end.
