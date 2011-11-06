%% Copyright ProcessOne 2006-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.

%% @author Jean-Sébastien Pédron <js.pedron@meetic-corp.com>

%% @doc
%% The module <strong>{@module}</strong> implements the receiving entity
%% side of Stream Compression (XEP-0138).
%%
%% @reference <a href="http://www.xmpp.org/extensions/xep-0138.html">XEP-0138: Stream Compression</a>
%% @reference <a href="http://www.xmpp.org/extensions/xep-0229.html">XEP-0229: Stream Compression with LZW</a>

-module(exmpp_server_compression).

-include("exmpp.hrl").

%% Feature announcement.
-export([
	 feature/1
	]).

%% Compression negotiation.
-export([
	 selected_method/1,
	 compressed/0,
	 failure/1
	]).

%% --------------------------------------------------------------------
%% Feature announcement.
%% --------------------------------------------------------------------

%% @spec (Methods) -> Feature
%%     Methods = [string()]
%%     Feature = exmpp_xml:xmlel()
%% @throws {stream_compression, feature_announcement, invalid_methods_list,
%%           []} |
%%         {stream_compression, feature_announcement, invalid_method, Method}
%% @doc Make a feature annoucement child.
%%
%% The `Methods' list must contain at least one method.
%%
%% Examples of methods are:
%% <ul>
%% <li>`"zlib"' (support required)</li>
%% <li>`"lzw"'</li>
%% </ul>
%%
%% The result should then be passed to {@link exmpp_stream:features/1}.

feature(Methods) ->
    #xmlel{ns = ?NS_COMPRESS_FEAT,
	   name = 'compression',
	   children = methods_list(Methods)
	  }.

methods_list([]) ->
    throw({stream_compression, feature_announcement,
	   invalid_methods_list, []});
methods_list(Methods) ->
    methods_list2(Methods, []).

methods_list2([Method | Rest], Children) ->
    case io_lib:deep_char_list(Method) of
        true ->
            Child = #xmlel{ns = ?NS_COMPRESS_FEAT,
			   name = 'method'
			  },
            methods_list2(Rest,
			  Children ++ [exmpp_xml:set_cdata(Child, Method)]);
        false ->
            throw({stream_compression, feature_announcement,
		   invalid_method, Method})
    end;
methods_list2([], Children) ->
    Children.

%% --------------------------------------------------------------------
%% Compression negotiation.
%% --------------------------------------------------------------------

standard_conditions() ->
    [
     {'unsupported-method'},
     {'setup-failed'}
    ].

%% @spec (El) -> Method
%%     El = exmpp_xml:xmlel()
%%     Method = string()
%% @doc Extract the method chosen by the initiating entity.

selected_method(#xmlel{ns = ?NS_COMPRESS, name = 'compress'} = El) ->
    case exmpp_xml:get_element(El, ?NS_COMPRESS, 'method') of
        undefined ->
            undefined;
        Sub_El ->
            exmpp_xml:get_cdata(Sub_El)
    end;
selected_method(El) ->
    throw({stream_compression, selected_method, unexpected_element, El}).

%% @spec () -> Compressed
%%     Compressed = exmpp_xml:xmlel()
%% @doc Prepare a `<compressed/>' element.

compressed() ->
    #xmlel{
	    ns = ?NS_COMPRESS,
	    name = 'compressed'
	   }.

%% @spec (Condition) -> Failure
%%     Condition = atom()
%%     Failure = exmpp_xml:xmlel()
%% @throws {stream_compression, failure, invalid_condition, Condition}
%% @doc Prepare a `<failure/>' element.

failure(Condition) ->
    case lists:keysearch(Condition, 1, standard_conditions()) of
        {value, _} ->
            Condition_El = #xmlel{
              ns = ?NS_COMPRESS,
              name = Condition
	     },
            #xmlel{ns = ?NS_COMPRESS,
		   name = failure,
		   children = [Condition_El]
		  };
        _ ->
            throw({stream_compression, failure, invalid_condition, Condition})
    end.
