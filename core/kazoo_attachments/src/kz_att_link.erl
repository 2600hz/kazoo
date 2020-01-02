%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2020, 2600Hz
%%% @doc DB Link for attachments.
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
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

%%==============================================================================
%% `gen_attachment' behaviour callbacks (API)
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec put_attachment(gen_attachment:settings()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ,gen_attachment:contents()
                    ,gen_attachment:options()
                    ) -> gen_attachment:put_response().
put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
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
            Routines = kz_att_error:put_routines(Params, DbName, DocId, AName, Contents,
                                                 Options),
            kz_att_error:new(Error, Routines)
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, DbName, DocId, AName) ->
    Resp =
        case kz_json:get_value(<<"att_name">>, HandlerProps) of
            'undefined' -> get_att_name(HandlerProps);
            AttName -> fetch_attachment(HandlerProps, AttName)
        end,
    Routines = kz_att_error:fetch_routines(HandlerProps, DbName, DocId, AName),
    handle_fetch_attachment_resp(Resp, Routines).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
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

-spec handle_fetch_attachment_resp(kz_datamgr:data_error(),
                                   kz_att_error:update_routines()) -> kz_att_error:error().
handle_fetch_attachment_resp({'ok', _Body} = Resp, _Routines) ->
    Resp;
handle_fetch_attachment_resp({'error', Reason} = _E, Routines) when not is_tuple(Reason) ->
    lager:debug("error fetching attachment: ~p", [_E]),
    kz_att_error:new(Reason, Routines);
handle_fetch_attachment_resp({'error', {Code, Body}} = _E, Routines) ->
    lager:debug("error fetching attachment: ~p", [_E]),
    NewRoutines = [{fun kz_att_error:set_resp_code/2, Code}
                  ,{fun kz_att_error:set_resp_body/2, Body}
                   | Routines
                  ],
    kz_att_error:new('request_error', NewRoutines);
handle_fetch_attachment_resp(_E, Routines) ->
    lager:debug("error fetching attachment: ~p", [_E]),
    kz_att_error:new('request_error', Routines).
