%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Socket Server.

-module(http_socket_server).
-author('ruslan@babayev.com').

-behaviour(gen_server).

%% external API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% internal API
-export([acceptor/3]).

-define(SERVER, ?MODULE). 

-record(state, {transport, listen, acceptor, loop}).

%% @doc Starts the server registered as Name with socket handler Loop.
%% @spec start_link(atom(), function()) -> {ok, Pid} | ignore | {error, Reason}
start_link(Name, Loop) ->
    gen_server:start_link({local, Name}, ?MODULE, [Loop], []).

%% @private
%% @doc Initializes the server.
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
init([Loop]) ->
    process_flag(trap_exit, true),
    {ok, Address} = application:get_env(ip),
    {ok, Port} = application:get_env(port),
    {ok, NoDelay} = application:get_env(nodelay),
    {ok, ReuseAddr} = application:get_env(reuseaddr),
    {ok, KeepAlive} = application:get_env(keepalive),
    {ok, Backlog} = application:get_env(backlog),
    SockOpts0 = [{mode, binary},
		 {active, false},
		 {ip, Address},
		 {reuseaddr, ReuseAddr},
		 {backlog, Backlog},
		 {nodelay, NoDelay},
		 {keepalive, KeepAlive}],
    {Transport, SockOpts1} =
	case application:get_env(ssl) of
	    {ok, SSLOpts} ->
		{ssl, SockOpts0 ++ SSLOpts};
	    undefined ->
		{gen_tcp, SockOpts0}
	end,
    case Transport:listen(Port, SockOpts1) of
	{ok, Socket} ->
	    {ok, new_acceptor(#state{listen = Socket, loop = Loop})};
	{error, Reason} ->
	    error_logger:error_report({?MODULE,Reason}),
	    {stop, Reason}
    end.

%% @private
%% @doc Handles call messages.
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                            {reply, Reply, State, Timeout} |
%%                                            {noreply, State} |
%%                                            {noreply, State, Timeout} |
%%                                            {stop, Reason, Reply, State} |
%%                                            {stop, Reason, State}
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
%% @doc Handles cast messages.
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast({accepted, _Pid}, State) ->
    {noreply, new_acceptor(State)}.

%% @private
%% @doc Handles all non call/cast messages.
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, closed}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, timeout}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{acceptor = Pid} = State) ->
    error_logger:error_report({?MODULE, acceptor_error, Reason}),
    {noreply, new_acceptor(State)};
handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:error_report({?MODULE, client_error, Reason}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @doc Performs cleanup on termination.
%% @spec terminate(Reason, State) -> any()
terminate(_Reason, #state{listen = Socket}) ->
    http_lib:close(Socket).

%% @private
%% @doc Converts process state when code is changed.
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
%% @doc Spawns a new acceptor.
%% @spec new_acceptor(#state{}) -> #state{}
new_acceptor(#state{listen = Listen, loop = Loop} = State) ->
    Pid = proc_lib:spawn_link(?MODULE, acceptor, [self(), Listen, Loop]),
    State#state{acceptor = Pid}.

%% @private
%% @doc Main acceptor loop.
%% @spec acceptor(pid(), Listen, function()) -> any()
acceptor(Server, Listen, Loop) ->
    case http_lib:accept(Listen) of
	{ok, Socket} ->
	    gen_server:cast(Server, {accepted, self()}),
	    Loop(Socket);
	{error, closed} ->
	    exit(normal);
	{error, Reason} ->
	    exit(Reason)
    end.
