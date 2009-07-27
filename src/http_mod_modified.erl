%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module handles requests that include `If-Modified-Since' and
%%% `If-Unmodified-Since' headers.
%%% Uses `file_info' flag.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_modified).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

init() ->
    ok.

handle(_Socket, #http_request{headers = Headers}, undefined, Flags) ->
    FileInfo = proplists:get_value(file_info, Flags),
    case proplists:get_value('If-Modified-Since', Headers) of
	undefined ->
	    case proplists:get_value('If-Unmodified-Since', Headers) of
		undefined ->
		    {proceed, undefined, Flags};
		Since ->
		    case http_lib:is_modified(FileInfo, Since) of
			true ->
			    http_lib:response(412);
			false ->
			    {proceed, undefined, Flags}
		    end
	    end;
	Since ->
	    case http_lib:is_modified(FileInfo, Since) of
		true ->
		    {proceed, undefined, Flags};
		false ->
		    http_lib:response(304)
	    end
    end;
handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.
