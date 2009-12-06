%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module handles the scheme request.
%%% Returns "Not Implemented" (501) response for now.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_scheme).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

init() ->
    ok.

handle(_Socket, #http_request{uri = #scheme{}}, _Response, _Flags) ->
    http_lib:response(501);
handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.
