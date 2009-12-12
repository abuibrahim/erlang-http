%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev
%% @doc HTTP Application.

-module(http_app).
-author('ruslan@babayev.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% @doc Starts the application.
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
start(_StartType, _StartArgs) ->
    http_sup:start_link().

%% @doc Stops the appolication.
%% @spec stop(State) -> void()
stop(_State) ->
    ok.
