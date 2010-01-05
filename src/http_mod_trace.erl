%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `TRACE' requests.

-module(http_mod_trace).
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
handle(_Socket, #http_request{method = 'TRACE'} = Request, _Response, _) ->
    Body = http_lib:encode(Request),
    Headers = [{'Content-Type', "message/http"},
	       {'Content-Length', iolist_size(Body)}],
    #http_response{headers = Headers, body = Body};
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.
