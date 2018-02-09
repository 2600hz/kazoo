%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-----------------------------------------------------------------------------

-module(kz_att_error).

-include("kz_att.hrl").

-export([new/1
        ,new/2
        ]).
-export([fetch_routines/4
        ,put_routines/6
        ]).
-export([db_name/1
        ,set_db_name/2
        ]).
-export([document_id/1
        ,set_document_id/2
        ]).
-export([attachment_name/1
        ,set_attachment_name/2
        ]).
-export([handler_props/1
        ,set_handler_props/2
        ]).
-export([req_url/1
        ,set_req_url/2
        ]).
-export([resp_body/1
        ,set_resp_body/2
        ]).
-export([resp_code/1
        ,set_resp_code/2
        ]).
-export([resp_headers/1
        ,set_resp_headers/2
        ]).

-type req_url() :: kz_term:ne_binary().
-type resp_code() :: pos_integer() | atom(). %% 400, 404, 409, etc.
-type resp_body() :: binary() | bitstring() | atom(). %% encoded map() | <<"example">> | 'not_found' | 'return_id_missing' | etc.
-type resp_headers() :: kz_term:proplist().

-type extended_error() :: #{db_name => gen_attachment:db_name()
                           ,document_id => gen_attachment:doc_id()
                           ,attachment_name => gen_attachment:att_name()
                           ,handler_props => gen_attachment:handler_props()
                           ,req_url => req_url()
                           ,resp_code => resp_code()
                           ,resp_body => resp_body()
                           ,resp_headers => resp_headers()
                           }.

-type update_routines() :: [{fun((extended_error(), Value) -> extended_error()), Value}].
-type error() :: {'error', kz_datamgr:data_errors(), extended_error()}.
-export_type([error/0
             ,update_routines/0
             ]).

-spec new(kz_datamgr:data_errors()) -> error().
new(Reason) ->
    new(Reason, []).

-spec new(kz_datamgr:data_errors(), update_routines()) -> error().
new(Reason, Routines) ->
    Extended = lists:foldl(fun({F, Value}, M) ->
                                   F(M, Value)
                           end, #{}, Routines),
    {'error', Reason, Extended}.

-spec fetch_routines(gen_attachment:handler_props()
                    ,gen_attachment:db_name()
                    ,gen_attachment:doc_id()
                    ,gen_attachment:att_name()
                    ) -> update_routines().
fetch_routines(HandlerProps, DbName, DocumentId, AttachmentName) ->
    [{fun kz_att_error:set_handler_props/2, HandlerProps}
    ,{fun kz_att_error:set_db_name/2, DbName}
    ,{fun kz_att_error:set_document_id/2, DocumentId}
    ,{fun kz_att_error:set_attachment_name/2, AttachmentName}
    ].

-spec put_routines(gen_attachment:settings()
                  ,gen_attachment:db_name()
                  ,gen_attachment:doc_id()
                  ,gen_attachment:att_name()
                  ,gen_attachment:contents()
                  ,gen_attachment:options()
                  ) -> update_routines().
put_routines(Settings, DbName, DocumentId, AttachmentName, Contents, Options) ->
    [{fun kz_att_error:set_handler_props/2, Settings}
    ,{fun kz_att_error:set_db_name/2, DbName}
    ,{fun kz_att_error:set_document_id/2, DocumentId}
    ,{fun kz_att_error:set_attachment_name/2, AttachmentName}
    ].

-spec db_name(extended_error()) -> gen_attachment:db_name().
db_name(#{db_name:=DbName}) ->
    DbName.

-spec set_db_name(extended_error(), gen_attachment:db_name()) -> extended_error().
set_db_name(ExtendedError, DbName) ->
    ExtendedError#{db_name => DbName}.

-spec document_id(extended_error()) -> gen_attachment:doc_id().
document_id(#{document_id:=DocumentId}) ->
    DocumentId.

-spec set_document_id(extended_error(), gen_attachment:doc_id()) -> extended_error().
set_document_id(ExtendedError, DocumentId) ->
    ExtendedError#{document_id => DocumentId}.

-spec attachment_name(extended_error()) -> gen_attachment:att_name().
attachment_name(#{attachment_name:=AttachmentName}) ->
    AttachmentName.

-spec set_attachment_name(extended_error(), gen_attachment:att_name()) -> extended_error().
set_attachment_name(ExtendedError, AttachmentName) ->
    ExtendedError#{attachment_name => AttachmentName}.

-spec handler_props(extended_error()) -> gen_attachment:handler_props().
handler_props(#{handler_props:=HandlerProps}) ->
    HandlerProps.

-spec set_handler_props(extended_error(), gen_attachment:handler_props()) -> extended_error().
set_handler_props(ExtendedError, HandlerProps) ->
    ExtendedError#{handler_props => HandlerProps}.

-spec req_url(extended_error()) -> req_url().
req_url(#{req_url:=Url}) ->
    Url.

-spec set_req_url(extended_error(), req_url()) -> req_url().
set_req_url(ExtendedError, Url) ->
    ExtendedError#{req_url => Url}.

-spec resp_body(extended_error()) -> resp_body().
resp_body(#{resp_body:=Body}) ->
    Body.

-spec set_resp_body(extended_error(), resp_body()) -> extended_error().
set_resp_body(ExtenedError, Body) ->
    ExtenedError#{resp_body => Body}.

-spec resp_code(extended_error()) -> resp_code().
resp_code(#{resp_code:=Code}) ->
    Code.

-spec set_resp_code(extended_error(), resp_code()) -> extended_error().
set_resp_code(ExtenedError, Code) ->
    ExtenedError#{resp_code => Code}.

-spec resp_headers(extended_error()) -> resp_headers().
resp_headers(#{resp_headers:=Headers}) ->
    Headers.

-spec set_resp_headers(extended_error(), resp_headers()) -> extended_error().
set_resp_headers(ExtendedError, Headers) ->
    ExtendedError#{resp_headers => Headers}.

    
