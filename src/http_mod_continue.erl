%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles clients that `Expect' `100-continue' status
%% before they send the request body.
%% @reference <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec8.html#sec8.2.3">Use of the 100 (Continue) Status</a>

-module(http_mod_continue).
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
handle(Socket, #http_request{method = M} = Request, Response, Flags, State)
  when M == 'POST'; M == 'PUT'; M == 'PROPFIND'; M == 'OPTIONS' ->
    case proplists:get_value("Expect", Request#http_request.headers) of
	"100-continue" ->
	    Continue = <<"HTTP/1.1 100 Continue\r\n\r\n">>,
	    http_lib:send(Socket, Continue),
	    {{proceed, Request, Response, Flags}, State};
	_ ->
	    {{proceed, Request, Response, Flags}, State}
    end;
handle(_Socket, Request, Response, Flags, State) ->
    {{proceed, Request, Response, Flags}, State}.
