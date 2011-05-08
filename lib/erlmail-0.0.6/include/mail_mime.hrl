%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Mail Mime library headers and record definitions.
%% @end

-ifndef(mail_mime).
-define(mail_mime, true).

-record(mime_msg, {headers = [], boundary, parts = []}).
-record(mime_part, {type,
                    encoding = {"7bit", "text/plain","iso-8859-1"},
                    name,
                    data}).

-endif.
