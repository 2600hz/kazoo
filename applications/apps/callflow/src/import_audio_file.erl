%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%------------------------------------------------------------------
-module(import_audio_file).

-export([to_couch/1]).

-define(DB, <<"system_media">>).

to_couch(Dir) ->
    couch_mgr:db_create(?DB),
    List = filelib:wildcard(Dir),
    lists:foreach(fun(File) ->
                          F = whistle_util:to_binary(File),
                          case couch_mgr:save_doc(?DB, create_doc(F)) of
                              {ok, JOBj} ->
                                  DocId = whapps_json:get_value(<<"_id">>, JOBj),
                                  AName = whapps_json:get_value(<<"display_name">>, JOBj),
                                  Rev = whapps_json:get_value(<<"_rev">>, JOBj),
                                  {ok, Content} = file:read_file(File),                                  
                                  couch_mgr:put_attachment(?DB, DocId, AName, Content, [{'content_type', "audio/x-wav"}, {'rev', Rev}]),
                                  io:format("File: ~p Return: ~p~n", [File, JOBj]);
                              _Else ->
                                  io:format("File: ~p ERROR: ~p~n", [File, _Else])
                          end
                  end, List).

create_doc(Path) ->
    PTokens = binary:split(Path, <<"/">>, [global, trim]),
    File = lists:last(PTokens),
    Id = hd(binary:split(File, <<".">>)),
    {struct, [
               {<<"_id">>, Id}
              ,{<<"pvt_type">>, <<"media">>}
              ,{<<"display_name">>, File}
              ,{<<"description">>, <<"">>}
             ]}.
