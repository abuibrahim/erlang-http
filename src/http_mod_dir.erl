%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module implements directory listing.
%%% Uses `path' and `file_info' flags.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_dir).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").

init() ->
    ok.

handle(_Socket, #http_request{method = 'GET', uri = URI}, undefined, Flags) ->
    Path = proplists:get_value(path, Flags),
    case proplists:get_value(file_info, Flags) of
	FileInfo when FileInfo#file_info.type == directory ->
	    ReqPath = http_lib:url_decode(http_lib:uri_to_path(URI)),
	    case list_dir(Path, ReqPath) of
		{ok, Dir} ->
		    Headers = [{'Content-Type', "text/html"},
			       {'Content-Length', iolist_size(Dir)}],
		    Response = #http_response{headers = Headers, body = Dir},
		    {proceed, Response, Flags};
		{error, _Reason} ->
		    http_lib:response(404)
	    end;
	_ ->
	    {proceed, undefined, Flags}
    end;
handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.

list_dir(AbsPath, ReqPath) ->
    case file:list_dir(AbsPath) of
	{ok, Entries} ->
	    SortedEntries = lists:sort(Entries),
	    Header = header(ReqPath),
	    Body = body(AbsPath, ReqPath, SortedEntries),
	    Footer = footer(),
	    {ok, [Header, Body, Footer]};
	{error, Reason} ->
	    {error, Reason}
    end.

header(ReqPath) ->
    Header =
	"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" "
	"\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"
	"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">\n"
	"<head>\n"
	"<title>Index of " ++ ReqPath ++ "</title>\n"
	"<style type=\"text/css\">\n"
	"a, a:active {text-decoration: none; color: blue;}\n"
	"a:visited {color: #48468F;}\n"
	"a:hover, a:focus {text-decoration: underline; color: red;}\n"
	"body {background-color: #F5F5F5;}\n"
	"h2 {margin-bottom: 12px;}\n"
	"table {margin-left: 12px;}\n"
	"th, td { font: 90% monospace; text-align: left;}\n"
	"th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;}\n"
	"td {padding-right: 14px;}\n"
	"td.s, th.s {text-align: right;}\n"
	"div.list { "
	"background-color: white; "
	"border-top: 1px solid #646464; "
	"border-bottom: 1px solid #646464; "
	"padding-top: 10px; "
	"padding-bottom: 14px;}\n"
	"div.foot { font: 90% monospace; color: #787878; padding-top: 4px;}\n"
	"</style>"
	"</head>\n"
	"<body>\n<h2>Index of " ++ ReqPath ++ "</h1>\n"
	"<div class=\"list\">\n"
	"<table summary=\"Directory Listing\""
	"cellpadding=\"0\" cellspasing=\"0\">\n"
	"<thead>"
	"<tr>"
	"<th class=\"n\">Name</th>"
	"<th class=\"m\">Last Modified</th>"
	"<th class=\"s\">Size</th>"
	"<th class=\"t\">Type</th>"
	"</tr>"
	"</thead>"
	"<tbody>",
    Header ++ parent(ReqPath).

parent("/") ->
    "";
parent(ReqPath) ->
    ParentReqPath =
	re:replace(strip(ReqPath), "[^/]*\$", "", [{return, list}]),
    "<tr><td class=\"n\"><a href=\"" ++ ParentReqPath ++ "\">..</a></td>"
	"<td class=\"m\"></td>"
	"<td class=\"s\">-</td>"
	"<td class=\"t\">Directory</td></tr>\n".

strip(Path) ->
    string:strip(Path, right, $/).

body(_AbsPath, _ReqPath, []) ->
    [];
body(AbsPath, ReqPath, [Entry | Rest]) ->
    [format(AbsPath, ReqPath, Entry) | body(AbsPath, ReqPath, Rest)].

format(AbsPath, ReqPath, Entry) ->
    case file:read_file_info(filename:join(AbsPath, Entry)) of
	{ok, FileInfo} when FileInfo#file_info.type == directory ->
	    tr(strip(ReqPath), Entry ++ "/", FileInfo);
	{ok, FileInfo} ->
	    tr(strip(ReqPath), Entry, FileInfo);
	{error, _Reason} ->
	    ""
    end.

tr(ReqPath, Entry, FileInfo) ->
    "<tr>"
	"<td class=\"n\">"
	"<a href=\"" ++ ReqPath ++ "/" ++ Entry ++"\">"	++ Entry ++ "</a>"
	"<td class=\"m\">" ++ mtime(FileInfo) ++ "</td>"
	"<td class=\"s\">" ++ filesize(FileInfo) ++ "</td>"
	"<td class=\"t\">" ++ filetype(FileInfo, Entry) ++ "</td></tr>\n".

mtime(#file_info{mtime = Time}) ->
    {{Year, Month, Day}, {Hour, Minute, _Second}} = Time,
    io_lib:format("~2.2.0w-~s-~w ~2.2.0w:~2.2.0w",
		  [Day, http_lib:month_to_list(Month), Year, Hour, Minute]).

filesize(#file_info{type = directory}) ->
    "-";
filesize(#file_info{size = Size}) ->
    io_lib:format("~w", [Size]).

filetype(#file_info{type = directory}, _Entry) ->
    "Directory";
filetype(#file_info{type = regular}, Entry) ->
    http_lib:mime_type(Entry, "-").

footer() ->
    "</tbody>\n"
	"</table>\n"
	"</div>\n"
	"<div class=\"foot\">erlang-http</div>\n"
	"</body>\n"
	"</html>\n".
