%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc HTTP Server.

-module(http_server).
-author('ruslan@babayev.com').

-export([start_link/0]).

-include("http.hrl").

-define(SERVER, ?MODULE).

-record(state, {socket,
		modules,
		request,
		response,
		headers = [],
		content_length = 0,
		chunk_size,
		chunk = <<>>,
		timeout}).

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link() ->
    {ok, Modules} = application:get_env(modules),
    {ok, Timeout} = application:get_env(idle_timeout),
    State = #state{modules = Modules, timeout = Timeout},
    Loop = fun(Socket) -> recv_request(State#state{socket = Socket}) end,
    case init(Modules) of
	ok ->
	    http_socket_server:start_link(?MODULE, Loop);
	{error, Reason} ->
	    error_logger:error_report({?MODULE, Reason}),
	    {stop, Reason}
    end.

%% @private
%% @doc Initializes modules.
%% @spec init(Modules::list()) -> ok | {error, Error}
init([]) ->
    ok;
init([Module | Rest]) ->
    case code:ensure_loaded(Module) of
	{module, Module} ->
	    case Module:init() of
		ok ->
		    init(Rest);
		{error, Reason} ->
		    {error, {Module, Reason}}
	    end;
	{error, Reason} ->
	    {error, {Module, Reason}}
    end.

recv_request(State) ->
    #state{socket = Socket, timeout = Timeout} = State,
    http_lib:setopts(Socket, [{packet, http}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, {http_request, Method, URI, Version}} ->
	    R = #http_request{method = Method, uri = URI, version = Version},
	    State1 = State#state{socket = Socket, request = R, headers = []},
	    recv_headers(State1);
	{ok, {http_error, "\r\n"}} ->
	    recv_headers(State);
	{ok, {http_error, "\n"}} ->
	    recv_headers(State);
	{error, closed} ->
	    http_lib:close(Socket),
	    exit(normal);
	{error, timeout} ->
	    http_lib:close(Socket),
	    exit(normal);
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

recv_headers(State) ->
    #state{socket = Socket, timeout = Timeout,
	   request = Request, headers = Headers} = State,
    http_lib:setopts(Socket, [{packet, httph}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, {http_header, _, Name, _, Val}} ->
	    recv_headers(State#state{headers = [{Name, Val} | Headers]});
	{ok, {http_error, "\r\n"}} ->
	    recv_headers(State);
	{ok, {http_error, "\n"}} ->
	    recv_headers(State);
	{ok, http_eoh} ->
	    R = Request#http_request{headers = lists:reverse(Headers)},
	    recv_body(State#state{request = R});
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

recv_body(#state{request = #http_request{method = M}} = State)
  when M == 'GET'; M == 'HEAD'; M == 'DELETE'; M == 'TRACE'; M == 'CONNECT';
       M == 'MOVE'; M == 'COPY'; M == 'MKCOL' ->
    handle_request(State);
recv_body(#state{socket = Socket, request = #http_request{method = M}} = State)
  when M == 'POST'; M == 'PUT'; M == 'PROPFIND'; M == 'OPTIONS' ->
    #http_request{headers = Headers} = State#state.request,
    case proplists:get_value("Expect", Headers) of
	"100-continue" ->
	    Continue = <<"HTTP/1.1 100 Continue\r\n\r\n">>,
	    case http_lib:send(Socket, Continue) of
		ok ->
		    recv_body2(State);
		{error, Reason} ->
		    http_lib:close(Socket),
		    exit(Reason)
	    end;
	_ ->
	    recv_body2(State)
    end.

recv_body2(#state{request = #http_request{headers = Headers}} = State) ->
    #state{socket = Socket} = State,
    case proplists:get_value('Content-Length', Headers) of
	undefined ->
	    case proplists:get_value('Transfer-Encoding', Headers) of
		undefined ->
		    recv_body3(State);
		"chunked" ->
		    recv_chunk_size(State#state{chunk = <<>>});
		_Unknown ->
		    NotImplemented = http_lib:response(501),
		    http_lib:send(Socket, http_lib:encode(NotImplemented)),
		    http_lib:close(Socket),
		    exit(normal)
	    end;
	Length ->
	    recv_body3(State#state{content_length = list_to_integer(Length)})
    end.

recv_body3(State) ->
    #state{socket = Socket, request = Request, timeout = Timeout,
	   content_length = ContentLength} = State,
    http_lib:setopts(Socket, [{packet, raw}]),
    case http_lib:recv(Socket, ContentLength, Timeout) of
	{ok, Body} ->
	    R = Request#http_request{body = Body},
	    handle_request(State#state{request = R, content_length = 0});
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

recv_chunk_size(State) ->
    #state{socket = Socket, timeout = Timeout} = State,
    http_lib:setopts(Socket, [{packet, line}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, Data} ->
	    F = fun(C) -> not lists:member(C, ";\r\n ") end,
	    Str = lists:takewhile(F, binary_to_list(Data)),
	    ChunkSize = erlang:list_to_integer(Str, 16),
	    State2 = State#state{chunk_size = ChunkSize},
	    recv_chunk(State2);
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

recv_chunk(#state{chunk_size = 0} = State) ->
    recv_trailers(State);
recv_chunk(#state{chunk_size = ChunkSize} = State) ->
    #state{socket = Socket, timeout = Timeout, chunk = Acc} = State,
    http_lib:setopts(Socket, [{packet, raw}]),
    case http_lib:recv(Socket, ChunkSize + 2, Timeout) of
	{ok, <<Chunk:ChunkSize/binary, "\r\n">>} ->
	    recv_chunk_size(State#state{chunk = list_to_binary([Acc, Chunk])});
	{ok, _Data} ->
	    http_lib:close(Socket),
	    exit(badchunk);
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

recv_trailers(State) ->
    #state{socket = Socket, timeout = Timeout, headers = Trailers,
	   request = Request, chunk = Acc} = State,
    http_lib:setopts(Socket, [{packet, line}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, <<"\r\n">>} ->
	    H = Request#http_request.headers ++ lists:reverse(Trailers),
	    R = Request#http_request{body = Acc, headers = H},
	    handle_request(State#state{request = R});
	{ok, Trailer} ->
	    case string:tokens(binary_to_list(Trailer), ": \r\n") of
		[Name, Value] ->
		    H = [{Name, Value} | Trailers],
		    recv_trailers(State#state{headers = H});
		_ ->
		    http_lib:close(Socket),
		    exit(badtrailer)
	    end;
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

handle_request(State) ->
    #state{modules = Modules, socket = Socket, request = Request} = State,
    case traverse(Socket, Request, Modules) of
	#http_response{} = Response ->
	    handle_response(State#state{response = Response});
	already_sent ->
	    handle_connection(State#state{response = undefined});
	{error, closed} ->
	    exit(normal);
	{error, Reason} ->
	    error_logger:error_report({?MODULE, Reason}),
	    InternalServerError = http_lib:response(500),
	    handle_response(State#state{response = InternalServerError});
	undefined ->
	    NotImplemented = http_lib:response(501),
	    handle_response(State#state{response = NotImplemented})
    end.

traverse(Socket, Request, Modules) ->
    traverse(Socket, Request, undefined, [], next, Modules).

traverse(_Socket, _Request, Response, _Flags, _Next, []) ->
    Response;
traverse(Socket, Request, Response, Flags, Next, [Module | Rest])
  when Next == Module; Next == next ->
    case Module:handle(Socket, Request, Response, Flags) of
	{proceed, Response1, Flags1} ->
	    traverse(Socket, Request, Response1, Flags1, next, Rest);
	{skip_to, Next1, Response1, Flags1} ->
	    traverse(Socket, Request, Response1, Flags1, Next1, Rest);
	Else ->
	    Else
    end;
traverse(Socket, Request, Response, Flags, Next, [_Module | Rest]) ->
    traverse(Socket, Request, Response, Flags, Next, Rest).

handle_response(#state{socket = Socket, response = Response} = State) ->
    case http_lib:send(Socket, http_lib:encode(Response)) of
	ok ->
	    handle_connection(State);
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

handle_connection(State) when State#state.response == undefined ->
    #state{socket = Socket, request = Request} = State,
    case http_lib:is_persistent(Request) of
	true ->
	    recv_request(State#state{request = undefined});
	false ->
	    http_lib:close(Socket),
	    exit(normal)
    end;
handle_connection(State) ->
    #state{socket = Socket, response = Response} = State,
    #http_response{headers = Headers} = Response,
    case proplists:get_value('Connection', Headers) of
	"close" ->
	    http_lib:close(Socket),
	    exit(normal);
	_ ->
	    handle_connection(State#state{response = undefined})
    end.
