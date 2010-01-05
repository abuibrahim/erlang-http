%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module implements ETS based cache.

-module(http_mod_cache).
-author('ruslan@babayev.com').

-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Initializes the module.
%% @spec init() -> ok | {error, Reason}
init() ->
    ets:new(http_cache, [set, public, named_table]),
    ok.

%% @doc Handles the Request, Response and Flags from previous modules.
%%      Uses `path' and `file_info' flags as well as `max_size_cached_file',
%%      `max_cache_size' and `max_cache_memory' environment variables.
%% @spec handle(Socket, Request, Response, Flags) -> Result
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       Proceed = {proceed, Request, Response, Flags}
handle(_Socket, #http_request{method = 'GET'} = Request, undefined, Flags) ->
    {ok, MaxSize} = application:get_env(max_size_cached_file),
    case proplists:get_value(file_info, Flags) of
	FI when FI#file_info.type == regular, FI#file_info.size < MaxSize ->
	    Path = proplists:get_value(path, Flags),
	    case ets:lookup(http_cache, Path) of
		[{Path, MTime, Bin}] when MTime == FI#file_info.mtime ->
		    %% a fresh cache entry found
		    H = headers(FI, Path),
		    Response = #http_response{headers = H, body = Bin},
		    {proceed, Request, Response, Flags};
		_ ->
		    %% cache entry is either missing or stale
		    case file:read_file(Path) of
			{ok, Bin} ->
			    Entry = {Path, FI#file_info.mtime, Bin},
			    insert(http_cache, Entry),
			    H = headers(FI, Path),
			    Response = #http_response{headers = H, body = Bin},
			    {proceed, Request, Response, Flags};
			{error, _Reason} ->
			    http_lib:response(404)
		    end
	    end;
	_ ->
	    {proceed, Request, undefined, Flags}
    end;
handle(_Socket, Request, Response, Flags) ->
    {proceed, Request, Response, Flags}.

headers(#file_info{mtime = Time, size = Size} = FileInfo, Path) ->
    Headers1 =
	[{'Content-Length', Size},
	 {'Last-Modified', http_lib:local_time_to_rfc1123(Time)},
	 {'Etag', http_lib:etag(FileInfo)}],
    case http_lib:mime_type(Path) of
	undefined ->
	    Headers1;
	MimeType ->
	    [{'Content-Type', MimeType} | Headers1]
    end.

insert(Cache, Entry) ->
    CacheSize = ets:info(Cache, size),
    case application:get_env(max_cache_size) of
	{ok, MaxCacheSize} when CacheSize > MaxCacheSize ->
	    ets:delete_all_objects(Cache),
	    ets:insert(Cache, Entry);
	_ ->
	    CacheMemory = ets:info(Cache, memory),
	    case application:get_env(max_cache_memory) of
		{ok, MaxCacheMemory} when CacheMemory > MaxCacheMemory ->
		    ets:delete_all_objects(Cache),
		    ets:insert(Cache, Entry);
		_ ->
		    ets:insert(Cache, Entry)
	    end
    end.
