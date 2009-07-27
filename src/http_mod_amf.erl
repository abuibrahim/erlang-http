%%%-------------------------------------------------------------------
%%% @author Ruslan Babayev <ruslan@babayev.com>
%%% @copyright 2009, Ruslan Babayev
%%% @doc This module handles Flex remoting requests.
%%% Uses `flex_services' and `flex_auth' environment variables.
%%% @end
%%%-------------------------------------------------------------------
-module(http_mod_amf).
-author('ruslan@babayev.com').
-export([init/0, handle/4, auth/2]).

-include("http.hrl").
-include_lib("amf/include/amf.hrl").

init() ->
    case application:start(amf) of
	ok ->
	    ok;
	{error, {already_started, amf}} ->
	    ok;
	_Else ->
	    {error, amf_not_found}
    end.

handle(_Socket, #http_request{method = 'POST'} = Request, Response, Flags) ->
    case proplists:get_value('Content-Type', Request#http_request.headers) of
	"application/x-amf" ->
	    AMFRequest = amf:decode_packet(Request#http_request.body),
	    AMFResponse = handle_amf_packet(AMFRequest),
	    Body = amf:encode_packet(AMFResponse),
	    Headers = [{'Content-Type', "application/x-amf"},
		       {'Content-Length', size(Body)}],
	    {proceed, #http_response{headers = Headers, body = Body}, Flags};
	_ ->
	    {proceed, Response, Flags}
    end;
handle(_Socket, _Request, Response, Flags) ->
    {proceed, Response, Flags}.

auth(Username, Password) ->
    error_logger:info_report({?MODULE, auth, Username, Password}),
    true.

handle_amf_packet(Request) ->
    Headers = handle_amf_headers(Request#amf_packet.headers, []),
    Messages = handle_amf_messages(Request#amf_packet.messages, []),
    Request#amf_packet{headers = Headers, messages = Messages}.

handle_amf_headers([], Acc) ->
    lists:reverse(Acc);
handle_amf_headers([_Header | Rest], Acc) ->
    handle_amf_headers(Rest, Acc).

handle_amf_messages([], Acc) ->
    lists:reverse(Acc);
handle_amf_messages([Message | Rest], Acc) ->
    handle_amf_messages(Rest, [handle_amf_message(Message) | Acc]).

handle_amf_message(#amf_message{response = Response, body = Body}) ->
    try handle_amf_message_body(Body) of
	Body1 ->
	    #amf_message{target = list_to_binary([Response, "/onResult"]),
			 response = <<>>,
			 body = Body1}
    catch
	Error ->
	    error_logger:warning_report({?MODULE, Error}),
	    #amf_message{target = list_to_binary([Response, "/onStatus"]),
			 response = <<>>,
			 body = error_message(Error)}
    end.

-define(SUBSCRIBE,                0).
-define(UNSUBSCRIBE,              1).
-define(POLL,                     2).
-define(CLIENT_SYNC,              4).
-define(CLIENT_PING,              5).
-define(CLUSTER_REQUEST,          7).
-define(LOGIN,                    8).
-define(LOGOUT,                   9).
-define(SUBSCRIPTION_INVALIDATE, 10).
-define(MULTI_SUBSCRIBE,         11).
-define(DISCONNECT,              12).
-define(TRIGGER_CONNECT,         13).
-define(UNKNOWN,              10000).

-define(COMMAND_MESSAGE,     <<"flex.messaging.messages.CommandMessage">>).
-define(ACKNOWLEDGE_MESSAGE, <<"flex.messaging.messages.AcknowledgeMessage">>).
-define(REMOTING_MESSAGE,    <<"flex.messaging.messages.RemotingMessage">>).
-define(ERROR_MESSAGE,       <<"flex.messaging.messages.ErrorMessage">>).

handle_amf_message_body([Object])
  when Object#amf_object.class == ?COMMAND_MESSAGE ->
    case proplists:get_value(operation, Object#amf_object.members) of
	?CLIENT_PING ->
	    acknowledge_message(null, Object);
	?DISCONNECT ->
	    acknowledge_message(null, Object);
	?LOGIN ->
	    Body = proplists:get_value(body, Object#amf_object.members),
	    Decoded =  base64:decode_to_string(Body),
	    [Username, Password] = re:split(Decoded, ":", [{parts, 2}]),
	    {ok, FlexAuth} = application:get_env(flex_auth),
	    case FlexAuth(Username, Password) of
		true ->
		    acknowledge_message(Username, Object);
		false ->
		    % @todo What to do we send when credentials are invalid?
		    acknowledge_message(Username, Object)
	    end;
	?LOGOUT ->
	    acknowledge_message(true, Object);
	?TRIGGER_CONNECT ->
	    acknowledge_message(null, Object);
	Else ->
	    throw({unsupported_operation, Else})
    end;
handle_amf_message_body([Object])
  when Object#amf_object.class == ?REMOTING_MESSAGE ->
    Members = Object#amf_object.members,
    Operation = binary_to_atom(proplists:get_value(operation, Members)),
    Source = binary_to_atom(proplists:get_value(source, Members)),
    Body = proplists:get_value(body, Members),
    {ok, FlexServices} = application:get_env(flex_services),
    case lists:member(Source, FlexServices) of
	true ->
	    Result = apply(Source, Operation, Body),
	    acknowledge_message(Result, Object);
	false ->
	    throw({unknown_service, Source})
    end.

binary_to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin)).

acknowledge_message(Body, #amf_object{members = Members}) ->
    MessageId = proplists:get_value(messageId, Members),
    ClientId = case proplists:get_value(clientId, Members, null) of
		   null ->
		       random_id();
		   Else ->
		       Else
	       end,
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = float((MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000),
    #amf_object{class = ?ACKNOWLEDGE_MESSAGE,
		members = [{messageId, random_id()},
			   {clientId, ClientId},
			   {correlationId, MessageId},
			   {destination, null},
			   {body, Body},
			   {timeToLive, 0},
			   {timestamp, Timestamp},
			   {headers, []}]}.

error_message(Reason) ->
    #amf_object{class = ?ERROR_MESSAGE,
		members = [{faultCode, format("~p", [Reason])},
			   {faultDetail, <<"Runtime Error">>},
			   {faultString, format("~p", [Reason])}]}.

format(Fmt, Args) ->
    list_to_binary(io_lib:format(Fmt, Args)).

random_id() ->
    <<X1:32, X2:16, X3:16, X4:16, X5:48>> = crypto:rand_bytes(16),
    IntToHex = fun(X) -> erlang:integer_to_list(X, 16) end,
    list_to_binary(string:join(lists:map(IntToHex, [X1,X2,X3,X4,X5]), "-")).
