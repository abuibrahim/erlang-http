-ifndef(_HTTP).
-define(_HTTP, true).

-define(HTTP_PORT, 80).

-type http_socket() :: port() | {sslsocket, any(), any()}.
-type http_port()   :: 1..65535.
-type http_method() :: 'POST' | 'GET' | 'HEAD' | 'PUT' | 'DELETE' | 'TRACE'.

-record(abs_path, {
	  path :: string()
	 }).

-record(absoluteURI, {
	  scheme :: http | https,
	  host   :: string(),
	  port   :: http_port(),
	  path   :: string()
	 }).

-record(scheme, {
	  scheme :: string(),
	  path   :: string()
	 }).

-type http_uri()     :: #abs_path{} | #absoluteURI{} | #scheme{}.
-type http_version() :: {integer(), integer()}.
-type http_headers() :: [{atom() | string(), string()}].
-type http_status()  :: integer().
-type http_body()    :: iolist() | binary().

-record(http_request, {
	  method = 'GET'  :: http_method(),
	  uri             :: http_uri(),
	  version = {1,1} :: http_version(),
	  headers = []    :: http_headers(),
	  body = <<>>     :: http_body()
	 }).

-record(http_response, {
	  version = {1,1} :: http_version(),
	  status = 200    :: http_status(),
	  headers = []    :: http_headers(),
	  body = <<>>     :: http_body()
	 }).

-type http_request()  :: #http_request{}.
-type http_response() :: #http_response{} | undefined.
-type proplist() :: [atom() | tuple()].

-endif.
