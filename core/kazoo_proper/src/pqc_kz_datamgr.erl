%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2022, 2600Hz
%%% @doc data adapter behaviour
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_kz_datamgr).

-export([seq/0, seq_kzoo_56/0
        ,cleanup/0
        ]).

-define(FROM_DB, <<"account%2F38%2F12%2F6201976b8666ab8c005daa7-from">>).
-define(FROM_DOC_ID, <<?MODULE_STRING, "-to-doc">>).
-define(TO_DB, <<"account%2F9f%2Fd0%2Fac025637b340164120d9e6c43-to">>).
-define(TO_DOC_ID, <<?MODULE_STRING, "-to-doc">>).

-spec seq() -> 'ok'.
seq() ->
    Fs = [fun seq_kzoo_56/0],
    lists:foreach(fun run_it/1, Fs).

-spec run_it(fun(() -> 'ok')) -> 'ok'.
run_it(F) -> F().

-spec seq_kzoo_56() -> 'ok'.
seq_kzoo_56() ->
    kz_datamgr:suppress_change_notice(),
    Doc = kz_json:from_list([{<<"_id">>, ?FROM_DOC_ID}
                             | [{kz_binary:rand_hex(4), kz_binary:rand_hex(5)} || _ <- lists:seq(1,10)]
                            ]),

    'true' = kz_datamgr:db_create(?FROM_DB, [{'publish_db', 'false'}]),
    'true' = kz_datamgr:db_create(?TO_DB, [{'publish_db', 'false'}]),

    {'ok', _SavedDoc} = kz_datamgr:save_doc(?FROM_DB, Doc),

    {'ok', MP3} = file:read_file(filename:join([code:priv_dir('kazoo_proper'), "mp3.mp3"])),

    _ = [save_mp3(?FROM_DB, ?FROM_DOC_ID, AttId, MP3) || AttId <- lists:seq(1, 3)],

    {'ok', SourceDoc} = kz_datamgr:open_doc(?FROM_DB, ?FROM_DOC_ID),

    {'ok', MovedDoc} = kz_datamgr:move_doc(?FROM_DB, ?FROM_DOC_ID, ?TO_DB, ?TO_DOC_ID, []),

    'true' = kz_doc:are_equal(SourceDoc, MovedDoc),
    lager:info("docs are the same"),

    cleanup().

-spec cleanup() -> 'ok'.
cleanup() ->
    'true' = kz_datamgr:db_delete(?FROM_DB),
    'true' = kz_datamgr:db_delete(?TO_DB),
    kz_datamgr:enable_change_notice(),
    lager:info("CLEANUP FINISHED").

save_mp3(DB, DocId, AttId, MP3) ->
    AttachmentName = <<"att-", (AttId+$0)>>,
    {'ok', _SavedWithAtt} = kz_datamgr:put_attachment(DB, DocId, AttachmentName, MP3).
