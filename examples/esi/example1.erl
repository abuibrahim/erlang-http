-module(example1).

-export([print1/3, print2/3]).

-include("http.hrl").

print1(_Socket, Request, Params) ->
    Body = io_lib:format("~p~nParams:~p", [Request, Params]),
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', iolist_size(Body)}],
    {proceed, Request, #http_response{headers = Headers, body = Body}, []}.

print2(_Socket, Request, Params) ->
    Body = io_lib:format("~p~nParams: ~p",[Request, Params]),
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', iolist_size(Body)},
	       {'Connection', "close"}],
    %% Returning #http_response{} indicates no further processing is required.
    #http_response{headers = Headers, body = Body}.
