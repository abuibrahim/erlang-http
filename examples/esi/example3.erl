-module(example3).
-export([location/3]).

-include("http.hrl").

location(_Socket, _Request, Params) ->
    URL = proplists:get_value(url, Params, "http://www.yahoo.com"),
    Headers = [{'Content-Length', 0}, {'Location', URL}],
    #http_response{status = 302, headers = Headers}.
