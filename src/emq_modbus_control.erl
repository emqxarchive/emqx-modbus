%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2016-2017 Feng Lee <feng@emqtt.io>, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------

-module(emq_modbus_control).

-include("emq_modbus.hrl").
-include_lib("emqttd/include/emqttd.hrl").

%% modbus API Exports
-export([start_link/2, stop/0, send_response/2]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        topic_prefix :: binary(),
        proto
 }).

-ifdef(TEST).
-define(PROTO_INIT(),                   (self() ! {keepalive, start, 10})).
-define(PROTO_SUBSCRIBE(X, Y),          test_broker_api:subscribe(X)).
-define(PROTO_PUBLISH(A1, A2, P),       test_broker_api:publish(A1, A2)).
-define(PROTO_DELIVER_ACK(A1, A2),      ok).
-define(PROTO_SHUTDOWN(A, B),           ok).
-else.
-define(PROTO_INIT(),                   emq_modbus_mqtt_adapter:proto_init()).
-define(PROTO_SUBSCRIBE(X, Y),          emq_modbus_mqtt_adapter:proto_subscribe(X, Y)).
-define(PROTO_PUBLISH(A1, A2, P),       emq_modbus_mqtt_adapter:proto_publish(A1, A2, P)).
-define(PROTO_DELIVER_ACK(Msg, State),  emq_modbus_mqtt_adapter:proto_deliver_ack(Msg, State)).
-define(PROTO_SHUTDOWN(A, B),           emqttd_protocol:shutdown(A, B)).
-endif.



%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------

start_link(CompanyName, EdgeName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CompanyName, EdgeName], []).

stop() ->
    gen_server:stop(?MODULE).

send_response(Control, Response) ->
    gen_server:cast(Control, {send_response, Response, self()}).

%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([CompanyName, EdgeName]) ->
    CompanyName1 = list_to_binary(CompanyName),
    EdgeName1 = list_to_binary(EdgeName),
    Topic  = <<"/", CompanyName1/binary,  "/modbus_request/", EdgeName1/binary, "/+">>,
    Prefix = <<"/", CompanyName1/binary,  "/modbus_response/", EdgeName1/binary, "/">>,
    Proto = ?PROTO_INIT(),
    NewProto = ?PROTO_SUBSCRIBE(Topic, Proto),

    {ok, #state{topic_prefix = Prefix, proto = NewProto}}.

handle_call(Req, _From, State) ->
    ?LOG(error, "unexpected call ~p", [Req]),
    {reply, {error, badreq}, State}.

handle_cast({send_response, Response, _From}, State=#state{topic_prefix = TopicPrefix, proto = Proto}) ->
    #modbus_rsp{device_name = DeviceName, msgid = MsgId, funcode = Funcode, data = Data} = Response,
    NewProto = ?PROTO_PUBLISH(<<TopicPrefix/binary, DeviceName/binary>>,
                                       <<MsgId:8, Funcode:8, Data/binary>>,
                                       Proto),
    {noreply, State#state{proto = NewProto}};

handle_cast(Msg, State) ->
    ?LOG(error, "unexpected cast ~p", [Msg]),
    {noreply, State}.

handle_info({deliver, Msg=#mqtt_message{topic = Topic, payload = Payload}}, State=#state{proto = Proto}) ->
    ?LOG(debug, "broker deliver Msg=~p", [Msg]),
    NewProto = ?PROTO_DELIVER_ACK(Msg, Proto),
    try
        <<MsgId:8, Code:8, Data/binary>> = Payload,
        TopicSplited = binary:split(Topic, <<"/">>, [global]),
        DeviceName = lists:last(TopicSplited),
        emq_modbus_device:send_request(DeviceName, #modbus_req{msgid = MsgId, funcode = Code, data = Data})
    catch
        Err -> ?LOG(error, "fail to dispatch message ~p, error=~p~n", [Topic, Err])
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






