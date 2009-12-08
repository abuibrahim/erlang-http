%% @author Ruslan Babayev <ruslan@babayev.com>
%% @copyright 2009, Ruslan Babayev
%% @doc This module handles Flex remoting requests.
%%      Uses `flex_services' and `flex_auth' environment variables.

-module(http_mod_amf).
-author('ruslan@babayev.com').

-export([init/0, handle/4, auth/2]).

-include("http.hrl").
%%-include_lib("amf/include/amf.hrl").
-include("amf.hrl").

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
	ResponseBody ->
	    #amf_message{target = list_to_binary([Response, "/onResult"]),
			 response = <<>>,
			 body = ResponseBody}
    catch
	Error ->
	    error_logger:error_report({?MODULE, Error}),
	    #amf_message{target = list_to_binary([Response, "/onStatus"]),
			 response = <<>>,
			 body = error_msg(Error)}
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
-define(ASYNC_MESSAGE,       <<"flex.messaging.messages.AsyncMessage">>).

handle_amf_message_body([{object, ?COMMAND_MESSAGE, Members} = Msg]) ->
    case proplists:get_value(operation, Members) of
	?CLIENT_PING ->
	    acknowledge_msg(Msg, null);
	?DISCONNECT ->
	    acknowledge_msg(Msg, null);
	?LOGIN ->
	    Body = proplists:get_value(body, Members),
	    Decoded =  base64:decode_to_string(Body),
	    [Username, Password] = re:split(Decoded, ":", [{parts, 2}]),
	    {ok, FlexAuth} = application:get_env(flex_auth),
	    case FlexAuth(Username, Password) of
		true ->
		    acknowledge_msg(Msg, Username);
		false ->
		    throw(invalid_credentials)
	    end;
	?LOGOUT ->
	    acknowledge_msg(Msg, true);
	?TRIGGER_CONNECT ->
	    acknowledge_msg(Msg, null);
	_Other ->
	    throw(unsupported_operation)
    end;
handle_amf_message_body([{object, ?REMOTING_MESSAGE, Members} = Msg]) ->
    Operation = binary_to_atom(proplists:get_value(operation, Members), utf8),
    Source = binary_to_atom(proplists:get_value(source, Members), utf8),
    Body = proplists:get_value(body, Members),
    {ok, FlexServices} = application:get_env(flex_services),
    case lists:member(Source, FlexServices) of
	true ->
	    try apply(Source, Operation, Body) of
		Result ->
		    acknowledge_msg(Msg, Result)
	    catch
		Class:Term ->
		    throw({service_failure, Class, Term})
	    end;
	false ->
	    throw(resource_unavailable)
    end;
handle_amf_message_body([{object, ?ASYNC_MESSAGE, Members} = Msg]) ->
    Destination =
	binary_to_atom(proplists:get_value(destination, Members), utf8),
    Headers = proplists:get_value(headers, Members),
    Body = proplists:get_value(body, Members),
    {ok, FlexDestinations} = application:get_env(flex_destinations),
    case lists:member(Destination, FlexDestinations) of
	true ->
	    try apply(Destination, 'send', [Headers, Body]) of
		Result ->
		    acknowledge_msg(Msg, Result)
	    catch
		Class:Term ->
		    throw({service_failure, Class, Term})
	    end;
	false ->
	    throw(resource_unavailable)
    end.

error_msg(invalid_credentials) ->
    error_msg(<<"Client.Authentication">>, <<"Invalid Credentials">>);
error_msg(resource_unavailable) ->
    error_msg(<<"Server.ResourceUnavailable">>, <<"Resource Unavailable">>);
error_msg(unsupported_operation) ->
    error_msg(<<"Server.Processing">>, <<"Unsupported Operation">>);
error_msg({service_failure, Class, Term}) ->
    FaultDetail = list_to_binary(io_lib:format("~p:~p", [Class, Term])),
    error_msg(<<"Server.Processing">>, FaultDetail, <<"Service Failure">>).

error_msg(FaultCode, FaultString) ->
    error_msg(FaultCode, <<"Runtime Error">>, FaultString).

error_msg(FaultCode, FaultDetail, FaultString) ->
    {object, ?ERROR_MESSAGE,
     [{faultCode, FaultCode},
      {faultDetail, FaultDetail},
      {faultString, FaultString}]}.

acknowledge_msg({object, _Class, Members}, Body) ->
    MessageId = proplists:get_value(messageId, Members),
    ClientId =
	case proplists:get_value(clientId, Members, null) of
	    null ->
		random_uuid();
	    Else ->
		Else
	end,
    Destination = proplists:get_value(destination, Members, null),
    {object, ?ACKNOWLEDGE_MESSAGE,
     [{messageId, random_uuid()},
      {clientId, ClientId},
      {correlationId, MessageId},
      {destination, Destination},
      {body, Body},
      {timeToLive, 0},
      {timestamp, now_to_milli_seconds(now())},
      {headers, []}]}.

random_uuid() ->
    <<X1:32, X2:16, X3:16, X4:16, X5:48>> = crypto:rand_bytes(16),
    IntToHex = fun(X) -> erlang:integer_to_list(X, 16) end,
    list_to_binary(string:join(lists:map(IntToHex, [X1,X2,X3,X4,X5]), "-")).

now_to_milli_seconds({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs / 1000.
