%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `GET' requests for regular files.

-module(http_mod_regular).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    {ok, MimeTypesConf} = application:get_env(mime_types),
    {ok, MimeTypes} = file:consult(http_lib:dir(MimeTypesConf)),
    ets:new(http_mime_types, [set, protected, named_table, {keypos, 2}]),
    ets:insert(http_mime_types, MimeTypes),
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `path' and `file_info' flags and `mime_types'
%%      environment variable.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(Socket, #http_request{method = 'GET'} = Request, undefined, Flags) ->
    case proplists:get_value(file_info, Flags) of
	FI when FI#file_info.type == regular ->
	    Path = proplists:get_value(path, Flags),
	    case file:open(Path, [raw, binary]) of
		{ok, IoDevice} ->
		    Size = FI#file_info.size,
		    LM = http_lib:local_time_to_rfc1123(FI#file_info.mtime),
		    Etag = http_lib:etag(FI),
		    H1 = [{'Content-Length', Size},
			  {'Last-Modified', LM},
			  {'Etag', Etag}],
		    H2  = case http_lib:mime_type(Path) of
			      undefined -> H1;
			      MimeType  -> [{'Content-Type', MimeType} | H1]
			  end,
		    Data = http_lib:encode(#http_response{headers = H2}),
		    http_lib:send(Socket, Data),
		    {ok, BlockSize} =
			application:get_env(large_file_block_size),
		    Result = send_in_blocks(Socket, IoDevice, BlockSize),
		    file:close(IoDevice),
		    Result;
		{error, _Reason} ->
		    http_lib:response(404)
	    end;
	_ ->
	    {proceed, Request, undefined, Flags}
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.

send_in_blocks(Socket, IoDevice, BlockSize) ->
    case file:read(IoDevice, BlockSize) of
	{ok, Bin} ->
	    case http_lib:send(Socket, Bin) of
		ok ->
		    send_in_blocks(Socket, IoDevice, BlockSize);
		Else ->
		    Else
	    end;
	eof ->
	    already_sent
    end.
