%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module implements Message Integrity Checking (MIC).
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_md5).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

init() ->
    ok.

handle(_Socket, _Request, undefined, Flags) ->
    {proceed, undefined, Flags};
handle(_Socket, _Request, Response, Flags)
  when is_record(Response, http_response) ->
    #http_response{body = Body, headers = Headers} = Response,
    Digest = base64:encode_to_string(erlang:md5(Body)),
    Headers1 = [{'Content-MD5', Digest} | Headers],
    Response1 = Response#http_response{headers = Headers1},
    {proceed, Response1, Flags}.
