-module(example2).

-export([chunked/3]).

-include("http.hrl").

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
