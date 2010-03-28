%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `OPTIONS' requests.

-module(http_mod_options).
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
handle(_Socket, #http_request{method = 'OPTIONS', uri = '*'}, _Response,
       _Flags, State) ->
    Headers = [{'Allow', "GET,HEAD,OPTIONS,PUT,POST,DELETE"}],
    {#http_response{headers = Headers}, State};
handle(_Socket, Request, Response, Flags, State) ->
    {{proceed, Request, Response, Flags}, State}.
