%%%-------------------------------------------------------------------
%%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------

-module(emqx_modbus_control).

-behaviour(gen_server).

-include("emqx_modbus.hrl").

-include_lib("emqx/include/emqx.hrl").

-include_lib("emqx/include/emqx_mqtt.hrl").

%% modbus API Exports
-export([start_link/2, stop/0, send_response/1]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { response_topic :: binary(),
          proto          :: term(),
          handler        :: atom(),
          device_module  :: atom()
        }).

-ifdef(TEST).
-define(PROTO_INIT(),                   (self() ! {keepalive, start, 10})).
-define(PROTO_SUBSCRIBE(X, Y),          test_mqtt_broker:subscribe(X)).
-define(PROTO_PUBLISH(A1, A2, P),       test_mqtt_broker:publish(A1, A2)).
-define(PROTO_DELIVER_ACK(A1, A2),      ok).
-define(PROTO_SHUTDOWN(A, B),           ok).
-else.
-define(PROTO_INIT(),                   emqx_modbus_mqtt_adapter:proto_init()).
-define(PROTO_SUBSCRIBE(X, Y),          emqx_modbus_mqtt_adapter:proto_subscribe(X, Y)).
-define(PROTO_PUBLISH(A1, A2, P),       emqx_modbus_mqtt_adapter:proto_publish(A1, A2, P)).
-define(PROTO_DELIVER_ACK(Msg, State),  emqx_modbus_mqtt_adapter:proto_deliver_ack(Msg, State)).
-define(PROTO_SHUTDOWN(A, B),           emqx_protocol:shutdown(A, B)).
-endif.

%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------

start_link(CompanyName, Mode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CompanyName, Mode], []).

stop() ->
    gen_server:stop(?MODULE).

send_response(Response) ->
    gen_server:cast(?MODULE, {send_response_to_broker, Response}).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([CompanyName, Mode]) ->
    Handler = application:get_env(?APP, handler, emqx_modbus_user_data),
    CompanyName1 = list_to_binary(CompanyName),
    Topic  = <<CompanyName1/binary,  <<"/modbus_request">>/binary>>,
    ResponseTopic = <<CompanyName1/binary,  <<"/modbus_response">>/binary>>,
    Proto = ?PROTO_INIT(),
    NewProto = ?PROTO_SUBSCRIBE(Topic, Proto),
    DevMod = case Mode of
                 1 -> emqx_modbus_device_mode1;
                 _ -> emqx_modbus_device_mode0
             end,

    {ok, #state{response_topic = ResponseTopic, proto = NewProto, handler = Handler, device_module = DevMod}}.

handle_call(Req, _From, State) ->
    ?LOG(error, "unexpected call ~p", [Req]),
    {reply, {error, badreq}, State}.

handle_cast({send_response_to_broker, Response}, State=#state{response_topic = ResponseTopic, proto = Proto}) ->
    NewProto = ?PROTO_PUBLISH(ResponseTopic, Response, Proto),
    {noreply, State#state{proto = NewProto}};

handle_cast(Msg, State) ->
    ?LOG(error, "unexpected cast ~p", [Msg]),
    {noreply, State}.

handle_info({deliver, Msg=#mqtt_message{topic = Topic, payload = Payload}},
            State=#state{proto = Proto, handler = Handler, device_module = DeviceModule}) ->
    ?LOG(debug, "receive broker delivered Msg=~p", [Msg]),
    NewProto = ?PROTO_DELIVER_ACK(Msg, Proto),
    try
        {DeviceName, Req} = Handler:handle_dl_data(Topic, Payload),
        DeviceModule:send_request(DeviceName, Req)
    catch
        _:Err -> ?LOG(error, "fail to send request to device, topic=~p, error=~p~n", [Topic, Err])
    end,
    {noreply, State#state{proto = NewProto}};

handle_info({keepalive,start,_Interval}, State) ->
    %% ignore keepalive, since this mqtt client should be always on
    %% to accept modbus request from cloud.
    {noreply, State};

handle_info({suback, _, _}, State) ->
    {noreply, State};

handle_info({subscribe,_}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(error, "unexpected info ~p", [Info]),
    {noreply, State}.

terminate(Reason, #state{proto = Proto}) ->
    ?PROTO_SHUTDOWN(Reason, Proto),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

