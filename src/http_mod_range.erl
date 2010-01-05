%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module handles `GET' requests containing `Range' header.

-module(http_mod_range).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

-define(BOUNDARY, "THIS_STRING_SEPARATES").
-define(READ_SIZE, 8*1024).

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `file_info' flag.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(Socket, #http_request{method = 'GET'} = Request, undefined, Flags) ->
    Headers = Request#http_request.headers,
    Path = proplists:get_value(path, Flags),
    FileInfo = proplists:get_value(file_info, Flags),
    case proplists:is_defined('Range', Headers) andalso
	is_unchanged(FileInfo, Headers) of
	true ->
	    Range = proplists:get_value('Range', Headers),
	    Size = FileInfo#file_info.size,
	    case parse(Range, Size) of
		{ok, Ranges} ->
		    send(Socket, Path, FileInfo, Ranges);
		{error, _Reason} ->
		    {proceed, Request, undefined, Flags}
	    end;
	false ->
	    {proceed, Request, undefined, Flags}
    end;
handle(_Socket, Request, undefined, Flags) ->
    {proceed, Request, undefined, Flags};
handle(_Socket, Request, Response, Flags)
  when is_record(Response, http_response) ->
    {proceed, Request, accept_ranges(Response), Flags}.

accept_ranges(Response) when is_record(Response, http_response) ->
    Headers = [{'Accept-Ranges', "bytes"} | Response#http_response.headers],
    Response#http_response{headers = Headers}.

is_unchanged(FileInfo, Headers) ->
    case proplists:get_value('If-Range', Headers) of
	undefined ->
	    true;
	IfRange ->
	    Since = http_lib:rfc1123_to_date_time(IfRange),
	    not http_lib:is_modified(FileInfo, Since)
    end.

parse("bytes=" ++ Range, Size) ->
    F = fun("-" ++ Last) ->
		Offset = list_to_integer(Last),
		validate(lists:max([0, Size - Offset]), Size - 1);
	   (R) ->
		case string:tokens(R, "-") of
		    [First, Last] ->
			Offset1 = list_to_integer(First),
			Offset2 = list_to_integer(Last),
			Offset3 = lists:min([Offset2, Size - 1]),
			validate(Offset1, Offset3);
		    [First] ->
			validate(list_to_integer(First), Size - 1)
		end
	end,
    {ok, lists:map(F, string:tokens(Range, ", "))};
parse(_, _) ->
    {error, badrange}.

validate(Offset1, Offset2) when Offset1 =< Offset2 ->
    {Offset1, Offset2};
validate(_, _) ->
    erlang:error(invalid_range).

send(_Socket, _Path, FileInfo, []) ->
    R = http_lib:response(416),
    H = R#http_response.headers,
    ContentRange = "bytes */" ++ integer_to_list(FileInfo#file_info.size),
    R#http_response{headers = [{'Content-Range', ContentRange} | H]};
send(Socket, Path, FileInfo, Ranges) ->
    MimeType = http_lib:mime_type(Path),
    Size = FileInfo#file_info.size,
    case file:open(Path, [raw, binary]) of
	{ok, IoDevice} ->
	    send_headers(Socket, FileInfo, MimeType, Ranges),
	    Result = send_ranges(Socket, IoDevice, MimeType, Size, Ranges),
	    file:close(IoDevice),
	    Result;
	{error, _Reason} ->
	    http_lib:response(404)
    end.

send_headers(Socket, FileInfo, MimeType, Ranges) ->
    H = headers(Ranges, FileInfo, MimeType),
    Data = http_lib:encode(#http_response{status = 206, headers = H}),
    http_lib:send(Socket, Data).

headers([SingleRange], FileInfo, MimeType) when MimeType /= undefined ->
    [{'Content-Type', MimeType} | headers([SingleRange], FileInfo)];
headers(Ranges, FileInfo, _MimeType) ->
    headers(Ranges, FileInfo).

headers([SingleRange], FileInfo) ->
    #file_info{mtime = Time, size = Size} = FileInfo,
    [{'Content-Length', Size},
     {'Accept-Ranges', "bytes"},
     {'Content-Range', range(SingleRange, Size)},
     {'Last-Modified', http_lib:local_time_to_rfc1123(Time)},
     {'Etag', http_lib:etag(FileInfo)}];
headers(_Ranges, FileInfo) ->
    #file_info{mtime = Time, size = Size} = FileInfo,
    [{'Content-Type', "multipart/byteranges; boundary=" ++ ?BOUNDARY},
     {'Content-Length', Size},
     {'Accept-Ranges', "bytes"},
     {'Last-Modified', http_lib:local_time_to_rfc1123(Time)},
     {'Etag', http_lib:etag(FileInfo)}].

range({From, To}, Size) ->
    lists:flatten(io_lib:format("bytes ~w-~w/~w", [From, To, Size])).

send_ranges(Socket, IoDevice, _MimeType, _Size, [SingleRange]) ->
    send_range(Socket, IoDevice, SingleRange);
send_ranges(Socket, IoDevice, MimeType, Size, Ranges) ->
    send_multipart_ranges(Socket, IoDevice, MimeType, Size, Ranges).

send_range(Socket, IoDevice, {From, To}) ->
    file:position(IoDevice, From),
    case file:read(IoDevice, chunk_size(From, To)) of
	{ok, Chunk} when size(Chunk) > 0 ->
	    case http_lib:send(Socket, Chunk) of
		ok ->
		    {ok, Current} = file:position(IoDevice, cur),
		    send_range(Socket, IoDevice, {Current, To});
		Else ->
		    Else
	    end;
	{ok, <<>>} ->
	    already_sent;
	eof ->
	    already_sent;
	Else ->
	    Else
    end.

chunk_size(From, To) when (From + ?READ_SIZE) =< To ->
    ?READ_SIZE;
chunk_size(From, To) ->
    To - From + 1.

send_multipart_ranges(Socket, _IoDevice, _MimeType, _Size, []) ->
    Footer = ["\r\n--", ?BOUNDARY, "--\r\n"],
    http_lib:send(Socket, Footer),
    already_sent;
send_multipart_ranges(Socket, IoDevice, MimeType, Size, [Range | Rest]) ->
    Headers = ["--", ?BOUNDARY, "\r\n",
	       "Content-Type: ", MimeType, "\r\n",
	       "Content-Range: ", range(Range, Size), "\r\n\r\n"],
    http_lib:send(Socket, Headers),
    case send_range(Socket, IoDevice, Range) of
	already_sent ->
	    Footer = ["\r\n--", ?BOUNDARY, "--\r\n"],
	    http_lib:send(Socket, Footer),
	    send_multipart_ranges(Socket, IoDevice, MimeType, Size, Rest);
	Else ->
	    Else
    end.
