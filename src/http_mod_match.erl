%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles requests containing `If-Match' or
%%      `If-None-Match' headers.

-module(http_mod_match).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `file_info' flag.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(_Socket, Request, undefined, Flags) ->
    #http_request{method = Method, headers = Headers} = Request,
    FileInfo = proplists:get_value(file_info, Flags),
    Etag = http_lib:etag(FileInfo),
    case proplists:get_value('If-Match', Headers) of
	undefined ->
	    case proplists:get_value('If-None-Match', Headers) of
		undefined ->
		    {proceed, Request, undefined, Flags};
		Etags ->
		    case member(Etag, string:tokens(Etags, ", ")) of
			false ->
			    {proceed, Request, undefined, Flags};
			true when Method == 'GET'; Method == 'HEAD' ->
			    http_lib:response(304);
			true ->
			    http_lib:response(412)
		    end
	    end;
	Etags ->
	    case member(Etag, string:tokens(Etags, ", ")) of
		true ->
		    {proceed, Request, undefined, Flags};
		false ->
		    http_lib:response(412)
	    end
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.

member(Element, List) ->
    lists:member("*", List) orelse lists:member(Element, List).
