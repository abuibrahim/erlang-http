%% @author Ruslan Babayev <ruslan@babayevcom>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Supervisor.

-module(http_sup).
-author('ruslan@babayev.com').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% @doc Starts the supervisor.
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
%% @doc Initializes the supervisor.
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
init([]) ->
    Client = {http_client, {http_client, start_link, []},
	      permanent, 2000, worker, [http_client]},
    Server = {http_server, {http_server, start_link, []},
	      permanent, 2000, worker, [http_server]},
    {ok, {{one_for_one, 3, 10}, [Client, Server]}}.

