%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `OPTIONS' requests.

-module(http_mod_options).
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
handle(_Socket, #http_request{method = 'OPTIONS', uri = '*'}, _Response, _F) ->
    Headers = [{'Allow', "GET,HEAD,OPTIONS,PUT,POST,DELETE"}],
    #http_response{headers = Headers};
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.
