%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module implements deflate compression for `GET' requests.
%%% Uses `path' flag.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_deflate).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

init() ->
    ok.

handle(_Socket, Request, Response, Flags)
  when Request#http_request.method == 'GET' ->
    Headers = Request#http_request.headers,
    Path = proplists:get_value(path, Flags),
    case accepts_deflate(Headers) andalso http_lib:is_compressible(Path) of
	true ->
	    ResponseHeaders = Response#http_response.headers,
	    case proplists:is_defined('Content-Encoding', ResponseHeaders) of
		false ->
		    {proceed, vary(deflate(Response)), Flags};
		true ->
		    {proceed, vary(Response), Flags}
	    end;
	false ->
	    {proceed, vary(Response), Flags}
    end;
handle(_Socket, _Request, undefined, Flags) ->
    {proceed, undefined, Flags};
handle(_Socket, _Request, Response, Flags)
  when is_record(Response, http_response) ->
    {proceed, vary(Response), Flags}.

accepts_deflate(Headers) ->
    case proplists:get_value('Accept-Encoding', Headers) of
	undefined ->
	    false;
	AcceptEncoding ->
	    lists:member("deflate", string:tokens(AcceptEncoding, ","))
    end.

vary(Response) when is_record(Response, http_response) ->
    case proplists:get_value('Vary', Response#http_response.headers) of
	"Accept-Encoding" ->
	    Response;
	_Else ->
	    Vary = {'Vary', "Accept-Encoding"},
	    Headers = [Vary | Response#http_response.headers],
	    Response#http_response{headers = Headers}
    end.

deflate(#http_response{body = Body, headers = Headers} = R) ->
    Z = zlib:open(),
    zlib:deflateInit(Z),
    Deflated = zlib:deflate(Z, Body, finish),
    DeflatedSize = iolist_size(Deflated),
    zlib:close(Z),
    case DeflatedSize < iolist_size(Body) of
	true ->
	    H1 = [{'Content-Encoding', "deflate"} | Headers],
	    H2 = keyreplace('Content-Length', H1, DeflatedSize),
	    H3 = lists:keydelete('Accept-Ranges', 1, H2),
	    R#http_response{status = 200, headers = H3, body = Deflated};
	false ->
	    R
    end.

keyreplace(Key, TupleList, NewValue) ->
    lists:keyreplace(Key, 1, TupleList, {Key, NewValue}).
