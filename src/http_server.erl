%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Server.

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
		timeout}).

%% @doc Starts the server.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Reason}
start_link() ->
    {ok, Modules} = application:get_env(modules),
    {ok, Timeout} = application:get_env(idle_timeout),
    State = #state{modules = Modules, timeout = Timeout},
    case init(Modules, []) of
	{ok, ModStates} ->
	    Loop = fun(Socket) ->
			   recv_request(State#state{socket = Socket,
						    modules = ModStates})
		   end,
	    http_socket_server:start_link(?MODULE, Loop);
	{error, Reason} ->
	    error_logger:error_report({?MODULE, Reason}),
	    {stop, Reason}
    end.

%% @private
%% @doc Initializes modules.
%% @spec init(Modules::list()) -> ok | {error, Reason}
init([], ModStates) ->
    {ok, lists:reverse(ModStates)};
init([Module | Rest], ModStates) ->
    case code:ensure_loaded(Module) of
	{module, Module} ->
	    case Module:init() of
		{ok, ModState} ->
		    init(Rest, [{Module, ModState} | ModStates]);
		{error, Reason} ->
		    {error, {Module, Reason}}
	    end;
	{error, Reason} ->
	    {error, {Module, Reason}}
    end.

recv_request(State) ->
    #state{socket = Socket, timeout = Timeout} = State,
    http_lib:setopts(Socket, [{packet,http}]),
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
    http_lib:setopts(Socket, [{packet,httph}]),
    case http_lib:recv(Socket, 0, Timeout) of
	{ok, {http_header, _, Name, _, Value}} ->
	    recv_headers(State#state{headers = [{Name,Value}|Headers]});
	{ok, {http_error, "\r\n"}} ->
	    recv_headers(State);
	{ok, {http_error, "\n"}} ->
	    recv_headers(State);
	{ok, http_eoh} ->
	    R = Request#http_request{headers = lists:reverse(Headers)},
	    handle_request(State#state{request = R});
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

handle_request(State) ->
    #state{modules = Modules, socket = Socket, request = Request} = State,
    {Response, NewModules} =
	traverse(Socket, Request, undefined, [], Modules, Modules),
    State1 = State#state{modules = NewModules},
    case Response of
	#http_response{} = Response ->
	    handle_response(State1#state{response = Response});
	already_sent ->
	    handle_connection(State1#state{response = undefined});
	{error, closed} ->
	    exit(normal);
	{error, Reason} ->
	    error_logger:error_report({?MODULE, Reason}),
	    InternalServerError = http_lib:response(500),
	    handle_response(State1#state{response = InternalServerError});
	undefined ->
	    NotImplemented = http_lib:response(501),
	    handle_response(State1#state{response = NotImplemented})
    end.

traverse(_Socket, _Request, Response, _Flags, [], NewModules) ->
    {Response, NewModules};
traverse(Socket, Request, Response, Flags, [{Module, State} | Rest],
	 NewModules) ->
    case Module:handle(Socket, Request, Response, Flags, State) of
	{{proceed, Request1, Response1, Flags1}, NewModState} ->
	    traverse(Socket, Request1, Response1, Flags1, Rest,
		     lists:keyreplace(Module, 1, NewModules,
				      {Module, NewModState}));
	{Result, NewModState} ->
	    {Result,
	     lists:keyreplace(Module, 1, NewModules, {Module, NewModState})};
	Other ->
	    exit({unexpected_value, Module, Other})
    end.

handle_response(#state{socket = Socket, response = Response} = State) ->
    case http_lib:send(Socket, http_lib:encode(Response)) of
	ok ->
	    handle_connection(State);
	{error, Reason} ->
	    http_lib:close(Socket),
	    exit(Reason)
    end.

handle_connection(#state{socket = Socket, request = Request} = State)
  when State#state.response == undefined ->
    case http_lib:is_persistent(Request) of
	true ->
	    recv_request(State#state{request = undefined});
	false ->
	    http_lib:close(Socket),
	    exit(normal)
    end;
handle_connection(#state{socket = Socket, response = Response} = State) ->
    case proplists:get_value('Connection', Response#http_response.headers) of
	"close" ->
	    http_lib:close(Socket),
	    exit(normal);
	_ ->
	    handle_connection(State#state{response = undefined})
    end.
