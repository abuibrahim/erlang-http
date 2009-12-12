%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module implements Server Side Includes (SSI).
%% @todo

-module(http_mod_include).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Error}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%% @spec handle(Socket, Request, Response, Flags) ->
%%       #http_response{} | already_sent | {error, Error} |
%%       {proceed, Request, Response, Flags}
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.
