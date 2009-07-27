-module(http_esi_example).
-export([print1/3, print2/3, chunked/3, location/3]).

-include("http.hrl").

print1(_Socket, Request, Params) ->
    Body = io_lib:format("~p~nParams:~p", [Request, Params]),
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', iolist_size(Body)}],
    {proceed, #http_response{headers = Headers, body = Body}, []}.

print2(_Socket, Request, Params) ->
    Body = io_lib:format("~p~nParams: ~p",[Request, Params]),
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', iolist_size(Body)},
	       {'Connection', "close"}],
    %% Returning #http_response{} indicates no further processing is required.
    #http_response{headers = Headers, body = Body}.

chunked(Socket, _Request, Params) ->
    Timeout = list_to_integer(proplists:get_value(timeout, Params, "1000")),
    Chunks = list_to_integer(proplists:get_value(chunks, Params, "10")),
    Headers = [{'Transfer-Encoding', "chunked"},
	       {'Content-Type', "text/plain"}],
    %% Sending headers. Body is empty if not specified.
    Response = #http_response{headers = Headers},
    %% Use http_lib:send/2 instead of gen_tcp:send/2 or ssl:send/2.
    http_lib:send(Socket, http_lib:encode(Response)),
    send_chunks(Socket, Timeout, 1, Chunks).

send_chunks(Socket, _Timeout, M, M) ->
    %% Send the last, empty chunk.
    http_lib:send(Socket, http_lib:chunk(<<>>)),
    already_sent;
send_chunks(Socket, Timeout, N, M) when N < M ->
    Chunk = http_lib:chunk(io_lib:format("chunk #~w~n", [N])),
    http_lib:send(Socket, Chunk),
    timer:sleep(Timeout),
    send_chunks(Socket, Timeout, N + 1, M).

location(_Socket, _Request, Params) ->
    URL = proplists:get_value(url, Params, "http://www.yahoo.com"),
    Headers = [{'Content-Length', 0}, {'Location', URL}],
    #http_response{status = 302, headers = Headers}.
