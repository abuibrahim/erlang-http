%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles Chunked Transfer Coding.
%% @reference <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1">Chunked Transfer Coding</a>

-module(http_mod_chunked).
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
handle(Socket, #http_request{method = M} = Request, Response, Flags)
  when M == 'POST'; M == 'PUT'; M == 'PROPFIND'; M == 'OPTIONS' ->
    Headers = Request#http_request.headers,
    case proplists:get_value('Transfer-Encoding', Headers) of
	undefined ->
	    {proceed, Request, Response, Flags};
	"chunked" ->
	    {ok, Timeout} = application:get_env(idle_timeout),
	    case recv_chunked_body(Socket, Timeout) of
		{ok, Body, Trailers} ->
		    Request1 = Request#http_request{
				 body = Body,
				 headers = Headers ++ Trailers
				},
		    {proceed, Request1, Response, Flags};
		{error, Reason} ->
		    {error, Reason}
	    end;
	_Other ->
	    http_lib:response(501)
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.

recv_chunked_body(Socket, Timeout) ->
    recv_chunk_size(Socket, Timeout, <<>>).

recv_chunk_size(Socket, Timeout, Acc) ->
    http_lib:setopts(Socket, [{packet,line}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, Data} ->
	    F = fun(C) -> not lists:member(C, ";\r\n ") end,
	    Str = lists:takewhile(F, binary_to_list(Data)),
	    Size = erlang:list_to_integer(Str, 16),
	    recv_chunk(Socket, Timeout, Size, Acc);
	{error, Reason} ->
	    {error, Reason}
    end.

recv_chunk(Socket, Timeout, 0, Acc) ->
    case recv_trailers(Socket, Timeout, []) of
	{ok, Trailers} ->
	    {ok, Acc, Trailers};
	{error, Reason} ->
	    {error, Reason}
    end;
recv_chunk(Socket, Timeout, Size, Acc) ->
    http_lib:setopts(Socket, [{packet,raw}]),
    case http_lib:recv(Socket, Size + 2, Timeout) of
	{ok, <<Chunk:Size/binary,"\r\n">>} ->
	    recv_chunk_size(Socket, Timeout, <<Acc/binary,Chunk/binary>>);
	{ok, _} ->
	    {error, badchunk};
	{error, Reason} ->
	    {error, Reason}
    end.

recv_trailers(Socket, Timeout, Trailers) ->
    http_lib:setopts(Socket, [{packet,line}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, <<"\r\n">>} ->
	    {ok, lists:reverse(Trailers)};
	{ok, Trailer} ->
	    case string:tokens(binary_to_list(Trailer), ": \r\n") of
		[Name,Value] ->
		    recv_trailers(Socket, Timeout, [{Name,Value}|Trailers]);
		_ ->
		    {error, badtrailer}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.
