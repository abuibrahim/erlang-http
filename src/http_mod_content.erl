%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `Content-Length' header.
%% @reference <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.13">Content-Length</a>

-module(http_mod_content).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(Socket, #http_request{method = M} = Request, Response, Flags)
  when M == 'POST'; M == 'PUT'; M == 'PROPFIND'; M == 'OPTIONS' ->
    Headers = Request#http_request.headers,
    case proplists:get_value('Content-Length', Headers) of
	undefined ->
	    {proceed, Request, Response, Flags};
	Length ->
	    {ok, Timeout} = application:get_env(idle_timeout),
	    http_lib:setopts(Socket, [{packet,raw}]),
	    case http_lib:recv(Socket, list_to_integer(Length), Timeout) of
		{ok, Body} ->
		    Request1 = Request#http_request{body = Body},
		    {proceed, Request1, Response, Flags};
		{error, Reason} ->
		    {error, Reason}
	    end
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.
