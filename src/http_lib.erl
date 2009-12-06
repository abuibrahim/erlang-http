%%-------------------------------------------------------------------
%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev
%% @doc HTTP Encoding and Utility Library.
%% @end
%%-------------------------------------------------------------------
-module(http_lib).
-export([uri_to_path/1, encode/1, list_to_absoluteURI/1, etag/1,
	 local_time_to_rfc1123/1, rfc1123_to_date_time/1, mime_type/1,
	 mime_type/2, extension/1, is_compressible/1, month_to_list/1,
	 url_decode/1, chunk/1, response/1, response/2,
	 accept/1, recv/2, recv/3, send/2, setopts/2, close/1, peername/1,
	 dir/1, is_persistent/1, is_idempotent/1, is_modified/2,
	 reason_phrase/1]).

-include("http.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Encodes HTTP request or response.
%% @spec encode(#http_request{} | #http_response{}) -> iolist()
%% @end
encode(#http_request{version = {Major, Minor}, method = Method,
		     uri = URI, headers = Headers, body = Body}) ->
    Headers1 = case proplists:is_defined('Host', Headers) of
		   true ->
		       Headers;
		   false when is_record(URI, absoluteURI) ->
		       [{'Host', URI#absoluteURI.host} | Headers];
		   false ->
		       Headers
	       end,
    [atom_to_list(Method), " ", uri_to_path(URI), " HTTP/",
     integer_to_list(Major), ".", integer_to_list(Minor), "\r\n",
     headers(Headers1), "\r\n", Body];
encode(#http_response{version = {Major, Minor}, status = Status,
		      headers = Headers0, body = Body}) ->
    LocalTime = calendar:now_to_local_time(now()),
    Date = local_time_to_rfc1123(LocalTime),
    {ok, Server} = application:get_env(http, server),
    Headers1 = [{'Date', Date}, {'Server', Server} | Headers0],
    ["HTTP/", integer_to_list(Major), ".", integer_to_list(Minor), " ",
     integer_to_list(Status), " ", reason_phrase(Status), "\r\n",
     headers(Headers1), "\r\n", Body].

uri_to_path(#abs_path{path = Path}) ->
    Path;
uri_to_path(#absoluteURI{path = Path}) ->
    Path;
uri_to_path('*') ->
    "*".

list_to_absoluteURI(List) ->
    RE = "^([^:/?#]+):?//([^/?#]*)",
    [_, S, HP, Rest]= re:split(List, RE, [{return, list}]),
    Scheme = list_to_atom(S),
    {Host, Port} =
	case re:split(HP, ":", [{return, list}]) of
	    [H, P] -> {H, list_to_integer(P)};
	    [H] when Scheme == http -> {H, 80};
	    [H] when Scheme == https -> {H, 443}
	end,
    Path = case Rest of
	       "" -> "/";
	       Else -> Else
	   end,
    #absoluteURI{scheme = Scheme, host = Host, port = Port, path = Path}.

headers(Headers) ->
    headers(Headers, []).

headers([], Acc) ->
    Acc;
headers([{Name, Value} | T], Acc) ->
    Header = [any_to_list(Name), ": ", any_to_list(Value), "\r\n"],
    headers(T, Acc ++ Header).

any_to_list(A) when is_atom(A) ->
    atom_to_list(A);
any_to_list(I) when is_integer(I) ->
    integer_to_list(I);
any_to_list(L) when is_list(L) ->
    L.

chunk(IoList) ->
    [erlang:integer_to_list(iolist_size(IoList), 16), "\r\n", IoList, "\r\n"].

local_time_to_rfc1123(LocalTime) ->
    [{{YYYY,MM,DD},{H,M,S}} | _] =
	calendar:local_time_to_universal_time_dst(LocalTime),
    Day = calendar:day_of_the_week({YYYY,MM,DD}),
    lists:flatten(
      io_lib:format("~s, ~2.2.0w ~3.s ~4.4.0w ~2.2.0w:~2.2.0w:~2.2.0w GMT",
		    [day(Day), DD, month_to_list(MM), YYYY, H, M, S])).

rfc1123_to_date_time([_D,_A,_Y,$,,$ ,D1,D2,$ ,M,O,N,$ ,Y1,Y2,Y3,Y4,$ ,
		      H1,H2,$:,M1,M2,$:,S1,S2|_]) ->
    Year = list_to_integer([Y1,Y2,Y3,Y4]),
    Day = list_to_integer([D1,D2]),
    Month = list_to_month([M,O,N]),
    Hour = list_to_integer([H1,H2]),
    Min = list_to_integer([M1,M2]),
    Sec = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}}.

day(1) -> "Mon";
day(2) -> "Tue";
day(3) -> "Wed";
day(4) -> "Thu";
day(5) -> "Fri";
day(6) -> "Sat";
day(7) -> "Sun".

month_to_list(1)  -> "Jan";
month_to_list(2)  -> "Feb";
month_to_list(3)  -> "Mar";
month_to_list(4)  -> "Apr";
month_to_list(5)  -> "May";
month_to_list(6)  -> "Jun";
month_to_list(7)  -> "Jul";
month_to_list(8)  -> "Aug";
month_to_list(9)  -> "Sep";
month_to_list(10) -> "Oct";
month_to_list(11) -> "Nov";
month_to_list(12) -> "Dec".

list_to_month("Jan") -> 1;
list_to_month("Feb") -> 2;
list_to_month("Mar") -> 3;
list_to_month("Apr") -> 4;
list_to_month("May") -> 5;
list_to_month("Jun") -> 6;
list_to_month("Jul") -> 7;
list_to_month("Aug") -> 8;
list_to_month("Sep") -> 9;
list_to_month("Oct") -> 10;
list_to_month("Nov") -> 11;
list_to_month("Dec") -> 12.

is_modified(#file_info{mtime = Mtime}, Since) ->
    erlang:localtime_to_universaltime(Mtime) > rfc1123_to_date_time(Since).

is_idempotent('HEAD')    -> true;
is_idempotent('GET')     -> true;
is_idempotent('PUT')     -> true;
is_idempotent('DELETE')  -> true;
is_idempotent('TRACE')   -> true;
is_idempotent('OPTIONS') -> true;
is_idempotent(_)         -> false.

is_persistent(#http_request{version = Version, headers = Headers}) ->
    Connection = proplists:get_value('Connection', Headers, ""),
    is_persistent(Version, string:to_lower(Connection)).

is_persistent(Version, _) when Version < {1,0} -> false;
is_persistent({1,0}, "keep-alive") -> true;
is_persistent({1,0}, _) -> false;
is_persistent(Version, "close") when Version >= {1,1} -> false;
is_persistent(Version, _) when Version >= {1,1} -> true.

url_decode(URL) ->
    url_decode(URL, []).

url_decode([], Acc) ->
    lists:reverse(Acc);
url_decode([37,H,L|T], Acc) ->
    url_decode(T, [erlang:list_to_integer([H,L], 16) | Acc]);
url_decode([$+|T], Acc) ->
    url_decode(T, [32|Acc]);
url_decode([H|T], Acc) ->
    url_decode(T, [H|Acc]).

url_decode_test() ->
    ?assertEqual("I am decoded", url_decode("I+am+decoded")),
    ?assertEqual("#$%&+=", url_decode("%23%24%25%26%2B%3Dm")).

%% @doc Generates Etag based on file modification time and size.
%% @spec etag(#file_info{}) -> string()
%% @end
etag(#file_info{mtime = MTime, size = Size}) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = MTime,
    F = fun(X) when X =< 25 -> X + $A;
	   (X) when X =< 50 -> X - 26 + $a;
	   (X) -> X - 3
	end,
    SizeStr = integer_to_list(Size),
    lists:map(F, [X rem 60 || X <- [Year,Month,Day,Hour,Min,Sec]]) ++ SizeStr.

mime_type(Path) ->
    mime_type(Path, undefined).

mime_type(Path, Default) ->
    Ext = extension(Path),
    case ets:lookup(http_mime_types, Ext) of
	[{mime_type, Ext, Type, _Compressible} | _] ->
	    Type;
	[] ->
	    Default
    end.

extension(Path) ->
    case filename:extension(Path) of
	[] ->
	    [];
	Extension ->
	    tl(Extension)
    end.

is_compressible(Path) ->
    Ext = extension(Path),
    case ets:lookup(http_mime_types, Ext) of
	[{mime_type, Ext, _Type, Compressible} | _] ->
	    Compressible;
	[] ->
	    true
    end.

response(Code) ->
    response(Code, reason_phrase(Code)).

response(Code, Reason) ->
    Headers = [{'Content-Type', "text/plain"},
	       {'Content-Length', iolist_size(Reason)}],
    #http_response{status = Code, headers = Headers, body = Reason}.

dir(priv_dir) ->
    code:priv_dir(http);
dir({priv_dir, App}) ->
    code:priv_dir(App);
dir({priv_dir, App, SubDir}) ->
    filename:join([code:priv_dir(App), any_to_list(SubDir)]);
dir({lib_dir, SubDir}) ->
    code:lib_dir(http, SubDir);
dir({lib_dir, App, SubDir}) ->
    code:lib_dir(App, SubDir);
dir(Path) when is_list(Path) ->
    Path.

accept(Socket) when element(1, Socket) == sslsocket ->
    case ssl:transport_accept(Socket) of
	{ok, SSLSocket} ->
	    case ssl:ssl_accept(SSLSocket) of
		ok ->
		    {ok, SSLSocket};
		Else ->
		    Else
	    end;
	Else ->
	    Else
    end;
accept(Socket) ->
    gen_tcp:accept(Socket).

recv(Socket, Length) when element(1, Socket) == sslsocket ->
    ssl:recv(Socket, Length);
recv(Socket, Length) ->
    gen_tcp:recv(Socket, Length).

recv(Socket, Length, Timeout) when element(1, Socket) == sslsocket ->
    ssl:recv(Socket, Length, Timeout);
recv(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).

send(Socket, Data) when element(1, Socket) == sslsocket ->
    ssl:send(Socket, Data);
send(Socket, Data) ->
    gen_tcp:send(Socket, Data).

setopts(Socket, Options) when element(1, Socket) == sslsocket ->
    ssl:setopts(Socket, Options);
setopts(Socket, Options) ->
    inet:setopts(Socket, Options).

close(Socket) when element(1, Socket) == sslsocket ->
    ssl:close(Socket);
close(Socket) ->
    gen_tcp:close(Socket).

peername(Socket) when element(1, Socket) == sslsocket ->
    ssl:peername(Socket);
peername(Socket) ->
    gen_tcp:peername(Socket).

%%% RFC 2616, HTTP 1.1 Status codes
reason_phrase(100) -> "Continue";
reason_phrase(101) -> "Switching Protocols";
reason_phrase(200) -> "OK";
reason_phrase(201) -> "Created";
reason_phrase(202) -> "Accepted";
reason_phrase(203) -> "Non-Authoritative Information";
reason_phrase(204) -> "No Content";
reason_phrase(205) -> "Reset Content";
reason_phrase(206) -> "Partial Content";
reason_phrase(300) -> "Multiple Choices";
reason_phrase(301) -> "Moved Permanently";
reason_phrase(302) -> "Moved Temporarily";
reason_phrase(303) -> "See Other";
reason_phrase(304) -> "Not Modified";
reason_phrase(305) -> "Use Proxy";
reason_phrase(306) -> "(unused)";
reason_phrase(307) -> "Temporary Redirect";
reason_phrase(400) -> "Bad Request";
reason_phrase(401) -> "Unauthorized";
reason_phrase(402) -> "Payment Required";
reason_phrase(403) -> "Forbidden";
reason_phrase(404) -> "Object Not Found";
reason_phrase(405) -> "Method Not Allowed";
reason_phrase(406) -> "Not Acceptable";
reason_phrase(407) -> "Proxy Authentication Required";
reason_phrase(408) -> "Request Time-out";
reason_phrase(409) -> "Conflict";
reason_phrase(410) -> "Gone";
reason_phrase(411) -> "Length Required";
reason_phrase(412) -> "Precondition Failed";
reason_phrase(413) -> "Request Entity Too Large";
reason_phrase(414) -> "Request-URI Too Large";
reason_phrase(415) -> "Unsupported Media Type";
reason_phrase(416) -> "Requested Range Not Satisfiable";
reason_phrase(417) -> "Expectation Failed";
reason_phrase(500) -> "Internal Server Error";
reason_phrase(501) -> "Not Implemented";
reason_phrase(502) -> "Bad Gateway";
reason_phrase(503) -> "Service Unavailable";
reason_phrase(504) -> "Gateway Time-out";
reason_phrase(505) -> "HTTP Version not supported";
%%% RFC 2518, HTTP Extensions for Distributed Authoring -- WEBDAV
reason_phrase(102) -> "Processing";
reason_phrase(207) -> "Multi-Status";
reason_phrase(422) -> "Unprocessable Entity";
reason_phrase(423) -> "Locked";
reason_phrase(424) -> "Failed Dependency";
reason_phrase(507) -> "Insufficient Storage";
%%% (Work in Progress) WebDAV Advanced Collections
reason_phrase(425) -> "";
%%% RFC 2817, HTTP Upgrade to TLS
reason_phrase(426) -> "Upgrade Required";
%%% RFC 3229, Delta encoding in HTTP
reason_phrase(226) -> "IM Used";
reason_phrase(_)   -> "Internal Server Error".
