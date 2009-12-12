%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009 Ruslan Babayev

-module(http).
-author('ruslan@babayev.com').

-export([start/0, stop/0]).

start() ->
    application:start(crypto),
    application:start(ssl),
    application:start(amf),
    application:start(http).

stop() ->
    application:stop(http).
