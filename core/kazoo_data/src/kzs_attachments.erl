%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% data adapter behaviour
%%% @end
%%% @contributors
%%%-----------------------------------------------------------------------------
-module(kzs_attachments).



%% Attachment-related
-export([fetch_attachment/4
         ,stream_attachment/5
         ,put_attachment/5
         ,put_attachment/6
         ,delete_attachment/4
         ,delete_attachment/5
        ]).


-include("kz_data.hrl").



%% Attachment-related functions ------------------------------------------------
-spec fetch_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                              {'ok', binary()} |
                              data_error().
fetch_attachment({App, Conn}, DbName, DocId, AName) ->
    App:fetch_attachment(Conn, DbName, DocId, AName).
    %% maybe translation here

-spec stream_attachment(server(), ne_binary(), ne_binary(), ne_binary(), pid()) ->
                               {'ok', reference()} |
                               data_error().
stream_attachment({App, Conn}, DbName, DocId, AName, Caller) ->
    App:stream_attachment(Conn, DbName, DocId, AName, Caller).
    %% maybe translation here and redirection

-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary()) ->
                            {'ok', wh_json:object()} |
                            data_error().
-spec put_attachment(server(), ne_binary(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                            {'ok', wh_json:object()} |
                            data_error().
put_attachment(Server, DbName, DocId, AName, Contents) ->
    put_attachment(Server, DbName, DocId, AName, Contents, []).

put_attachment({App, Conn}, DbName, DocId, AName, Contents, Options) ->
    %% maybe translation here and redirection
    App:put_attachment(Conn, DbName, DocId, AName, Contents, Options).

-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary()) ->
                               {'ok', wh_json:object()} |
                               data_error().
-spec delete_attachment(server(), ne_binary(), ne_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               data_error().
delete_attachment(Server, DbName, DocId, AName) ->
    delete_attachment(Server, DbName, DocId, AName, []).

delete_attachment({App, Conn}, DbName, DocId, AName, Options) ->
    App:delete_attachment(Conn, DbName, DocId, AName, Options).
