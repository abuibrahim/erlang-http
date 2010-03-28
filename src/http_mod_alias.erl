%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc This module maps the request URI to a path.

-module(http_mod_alias).
-author('ruslan@babayev.com').

-export([init/0, handle/5]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

%% @doc Initializes the module.
%% @spec init() -> {ok, State} | {error, Reason}
init() ->
    {ok, undefined}.

%% @doc Handles the Request, Response and Flags from previous modules.
%% @todo Returns "Not Implemented" (501).
%% @spec handle(Socket, Request, Response, Flags, State) -> {Result, NewState}
%%       Request = #http_request{}
%%       Response = #http_response{} | undefined
%%       Flags = list()
%%       Result = #http_response{} | already_sent | {error, Reason} | Proceed
%%       NewState = any()
%%       Proceed = {proceed, Request, Response, Flags}
handle(_Socket, Request, Response, Flags, State) ->
    ReqPath = http_lib:uri_to_path(Request#http_request.uri),
    DecodedPath = http_lib:url_decode(ReqPath),
    {ok, Aliases} = application:get_env(aliases),
    {ok, DocRoot} = application:get_env(docroot),
    Path1 = abs_path(DecodedPath, Aliases, http_lib:dir(DocRoot)),
    {ok, Indices} = application:get_env(indices),
    Path2 = maybe_append_index(Path1, Indices),
    [Path3|_] = string:tokens(Path2, "?"),
    {{proceed, Request, Response, [{path,Path3}|Flags]}, State}.

abs_path(ReqPath, [], DocRoot) ->
    DocRoot ++ ReqPath;
abs_path(ReqPath, [{Alias,Path}|Rest], DocRoot) ->
    case string:str(ReqPath, Alias) of
	1 ->
	    http_lib:dir(Path) ++ string:substr(ReqPath, length(Alias) + 1);
	_ ->
	    abs_path(ReqPath, Rest, DocRoot)
    end.

maybe_append_index(Path, Indices) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    append_index(Path, Indices);
	_ ->
	    Path
    end.

append_index(Path, []) ->
    Path;
append_index(Path, [Index|Rest]) ->
    case file:read_file_info(filename:join(Path, Index)) of
	{error, _Reason} ->
	    append_index(Path, Rest);
	_ ->
	    filename:join(Path, Index)
    end.
