%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module implements Server Side Includes (SSI).
%%% @todo
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_include).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

init() ->
    ok.

handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.
