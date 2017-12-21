%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% Simple Url Storage for attachments
%%% @end
%%% @contributors
%%%   Luis Azedo
%%%-----------------------------------------------------------------------------
-module(kz_att_http).
-behaviour(gen_attachment).

-include("kz_att.hrl").

%% `gen_attachment' behaviour callbacks (API)
-export([put_attachment/6]).
-export([fetch_attachment/4]).

-define(MAX_REDIRECTS, 10).

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
put_attachment(Params, DbName, DocId, AName, Contents, Options) ->
    #{url := BaseUrlParam, verb := Verb} = Params,
    DocUrlField = maps:get('document_url_field', Params, 'undefined'),
    BaseUrl = kz_binary:strip_right(BaseUrlParam, $/),
    Url = list_to_binary([BaseUrl, base_separator(BaseUrl), kz_att_util:format_url(Params, {DbName, DocId, AName})]),
    Headers = [{'content_type', props:get_value('content_type', Options, kz_mime:from_filename(AName))}],

    case send_request(Url, kz_term:to_atom(Verb, 'true'), Headers, Contents) of
        {'ok', NewUrl, _Body, _Debug} -> {'ok', url_fields(DocUrlField, NewUrl)};
        Error -> Error
    end.

-spec fetch_attachment(gen_attachment:handler_props()
                      ,gen_attachment:db_name()
                      ,gen_attachment:doc_id()
                      ,gen_attachment:att_name()
                      ) -> gen_attachment:fetch_response().
fetch_attachment(HandlerProps, _DbName, _DocId, _AName) ->
    case kz_json:get_value(<<"url">>, HandlerProps) of
        'undefined' -> gen_attachment:error_response(400, 'invalid_data');
        Url -> fetch_attachment(Url)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
-spec base_separator(kz_term:ne_binary()) -> binary().
base_separator(Url) ->
    case kz_http_util:urlsplit(Url) of
        {_, _, _, <<>>, _} -> <<"/">>;
        {_, _, _, _, _} -> <<>>
    end.

send_request(Url, Verb, Headers, Contents) ->
    send_request(Url, Verb, Headers, Contents, 0, kz_json:new()).

send_request(_Url, _Verb, _Headers, _Contents, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    gen_attachment:error_response(400, 'too_many_redirects');
send_request(Url, Verb, Headers, Contents, Redirects, Debug) ->
    case kz_http:req(Verb, Url, Headers, Contents) of
        {'ok', Code, ReplyHeaders, Body} when
              is_integer(Code)
              andalso Code >= 200
              andalso Code =< 299 ->
            {'ok', Url, Body, add_debug(Debug, Url, Code, ReplyHeaders)};
        {'ok', Code, ReplyHeaders, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, ReplyHeaders),
            Fun = fun(URL, Tries, Data) -> send_request(URL, Verb, Headers, Contents, Tries, Data) end,
            maybe_redirect(Url, ReplyHeaders, Redirects, NewDebug, Fun);
        Resp ->
            kz_att_util:handle_http_error_response(Resp, "HTTP error", Url)
    end.

maybe_redirect(ToUrl, Headers, Redirects, Debug, Fun) ->
    case props:get_value("location", Headers) of
        'undefined' ->
            lager:debug("request to ~s got redirection but no 'location' header was found", [ToUrl]),
            gen_attachment:error_response(400, 'redirection_with_no_location');
        Url ->
            maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun)
    end.

maybe_redirect_loop(ToUrl, Url, Redirects, Debug, Fun) ->
    case kz_json:get_value(Url, Debug) of
        'undefined' ->
            lager:debug("request to ~s redirected to ~s.",[ToUrl, Url]),
            Fun(Url, Redirects + 1, Debug);
        _ ->
            lager:debug("the request ~s got redirect to ~s but this is already in the list of visited urls",[ToUrl, Url]),
            gen_attachment:error_response(400, 'redirect_loop_detected')
    end.

add_debug(Debug, Url, Code, Headers) ->
    kz_json:set_values([{[Url, <<"code">>], Code}
                       ,{[Url, <<"headers">>], kz_json:from_list(Headers)}
                       ], Debug).


url_fields('undefined', Url) ->
    [{'attachment', [{<<"url">>, Url}]}];
url_fields(DocUrlField, Url) ->
    [{'attachment', [{<<"url">>, Url}]}
    ,{'document', [{DocUrlField, Url}]}
    ].

fetch_attachment(URL) ->
    fetch_attachment(URL, 0, kz_json:new()).

-spec fetch_attachment(kz_term:ne_binary(), integer(), kz_json:object()) ->
                              gen_attachment:fetch_response().
fetch_attachment(_Url, Redirects, _)
  when Redirects > ?MAX_REDIRECTS ->
    gen_attachment:error_response(400, 'too_many_redirects');
fetch_attachment(Url, Redirects, Debug) ->
    case kz_http:get(Url) of
        {'ok', 200, _Headers, Body} ->
            {'ok', Body};
        {'ok', Code, Headers, _Body} when
              Code =:= 301;
              Code =:= 302 ->
            NewDebug = add_debug(Debug, Url, Code, Headers),
            Fun = fun(URL, N, Data) -> fetch_attachment(URL, N, Data) end,
            maybe_redirect(Url, Headers, Redirects, NewDebug, Fun);
        Resp ->
            kz_att_util:handle_http_error_response(Resp, "HTTP fetch error", Url)
    end.
