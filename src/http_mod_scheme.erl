%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles the scheme request.
%%      Returns "Not Implemented" (501) response for now.

-module(http_mod_scheme).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Error}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%% @spec handle(Socket, Request, Response, Flags) ->
%%       #http_response{} | already_sent | {error, Error} |
%%       {proceed, Request, Response, Flags}
handle(_Socket, #http_request{uri = #scheme{}}, _Response, _Flags) ->
    http_lib:response(501);
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.
