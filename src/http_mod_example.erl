-module(http_mod_example).
-author('ruslan@babayev.com').
-export([init/0, handle/4]).

-include("http.hrl").

%% @spec () -> ok | error
%% @doc Returns `ok' if the module initialization succeeds, `error' otherwize.
-spec init() -> ok | error.

init() ->
    ok.

%% @spec handle(Socket, Request, Response, Flags) ->
%%           {proceed, NewResponse, NewFlags} |
%%           {skip_to, Module, NewResponse, NewFlags} |
%%           Response |
%%           already_sent |
%%           {error, closed} |
%%           {error, Reason}
%% where
%%       Socket = http_socket()
%%       Request = #http_request{}
%%       Response = #http_response{}
%%       Flags = proplist()
%%       NewResponse = #http_response{}
%%       NewFlags = proplist()
%%       Module = atom()
%%       Reason = string()
%%
%% @type http_socket() = port() | #sslsocket{}
%% @type proplist() = [atom() | tuple()]
%%
%% @doc The entry point to the module.
%%
%% Returns one of the following:
%% <dl>
%%  <dt>`{proceed, Response, Flags}'
%%  </dt>
%%  <dd>Pass the `Response' and the `Flags' to the next module in
%%      the `modules' list. If this was the last module, send the `Response'
%%      to the client socket.
%%  </dd>
%%  <dt>`{skip_to, Module, Response, Flags}'
%%  </dt>
%%  <dd>Skip to the specified `Module'. If `Module' is not found, send the
%%      `Response' to the client socket.
%%  </dd>
%%  <dt>`#http_response{}'
%%  </dt>
%%  <dd>Modules traversal is complete. Response will be sent to the socket.
%%  </dd>
%%  <dt>`already_sent'
%%  </dt>
%%  <dd>Modules traversal is complete. The response has already been
%%      sent to the client socket.
%%  </dd>
%%  <dt>`{error, closed}'
%%  </dt>
%%  <dd>Client socket was closed. Exit the handler.
%%  </dd>
%%  <dt>`{error, Reason::string()}'
%%  </dt>
%%  <dd>Internal Server Error (500) will be sent to the client.
%%      The error Reason will be logged.
%%  </dd>
%% </dl>
-spec handle(http_socket(), http_request(), http_response(), proplist()) ->
    {proceed, http_response(), proplist()} |
    {skip_to, atom(), http_response(), proplist()} |
    http_response() |
    already_sent |
    {error, string()}.

handle(_Socket, #http_request{uri = URI}, _Response, _Flags)
  when is_record(URI, scheme) ->
    http_lib:response(501);
handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.
