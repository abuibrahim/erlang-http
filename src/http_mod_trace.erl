%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `TRACE' requests.

-module(http_mod_trace).
-author('ruslan@babayev.com').

-export([init/0, handle/5]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> {ok, State} | {error, Reason}
init() ->
    {ok, undefined}.

%% @doc Handles the Request, Response and Flags from previous modules.
%% @todo Returns "Not Implemented" (501).
%% @spec handle(Socket, Request, Response, Flags, State) -> {Result, NewState}
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       NewState = any()
%%       Proceed = {proceed, Request, Response, Flags}
handle(_Socket, #http_request{method = 'TRACE'} = Request, _Response,
       _Flags, State) ->
    Body = http_lib:encode(Request),
    Headers = [{'Content-Type', "message/http"},
	       {'Content-Length', iolist_size(Body)}],
    {#http_response{headers = Headers, body = Body}, State};
handle(_Socket, Request, Response, Flags, State) ->
    {{proceed, Request, Response, Flags}, State}.
