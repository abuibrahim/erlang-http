%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles requests containing `If-Modified-Since' or
%%      `If-Unmodified-Since' headers.

-module(http_mod_modified).
-author('ruslan@babayev.com').

-export([init/0, handle/5]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> {ok, State} | {error, Reason}
init() ->
    {ok, undefined}.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `file_info' flag.
%% @spec handle(Socket, Request, Response, Flags, State) -> {Result, State}
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       NewState = any()
%%       Proceed = {proceed, Request, Response, Flags}
handle(_Socket, Request, undefined, Flags, State) ->
    FileInfo = proplists:get_value(file_info, Flags),
    #http_request{headers = Headers} = Request,
    case proplists:get_value('If-Modified-Since', Headers) of
	undefined ->
	    case proplists:get_value('If-Unmodified-Since', Headers) of
		undefined ->
		    {{proceed, Request, undefined, Flags}, State};
		Since ->
		    case http_lib:is_modified(FileInfo, Since) of
			true ->
			    {http_lib:response(412), State};
			false ->
			    {{proceed, Request, undefined, Flags}, State}
		    end
	    end;
	Since ->
	    case http_lib:is_modified(FileInfo, Since) of
		true ->
		    {{proceed, Request, undefined, Flags}, State};
		false ->
		    {{http_lib:response(304), State}, State}
	    end
    end;
handle(_Socket, Request, Response, Flags, State) ->
    {{proceed, Request, Response, Flags}, State}.
