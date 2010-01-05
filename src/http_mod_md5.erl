%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module implements Message Integrity Checking (MIC).

-module(http_mod_md5).
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
handle(_Socket, Request, undefined, Flags) ->
    {proceed, Request, undefined, Flags};
handle(_Socket, Request, Response, Flags)
  when is_record(Response, http_response) ->
    #http_response{body = Body, headers = Headers} = Response,
    Digest = base64:encode_to_string(erlang:md5(Body)),
    Headers1 = [{'Content-MD5', Digest} | Headers],
    Response1 = Response#http_response{headers = Headers1},
    {proceed, Request, Response1, Flags}.
