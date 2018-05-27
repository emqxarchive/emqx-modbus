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

-module(emqx_modbus_device_mode0).

-behaviour(gen_server).

-include("emqx_modbus.hrl").

%% esockd callback 
-export([start_link/2, send_request/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transport, sock, handler, parser, device_name, msgid, keepalive}).

-define(KEEPALIVE_START(X),   emqx_modbus_keepalive:start_timer(X, keepalive_timer)).
-define(KEEPALIVE_RESTART(X), emqx_modbus_keepalive:restart_timer(X)).
-define(KEEPALIVE_TIMEOUT(X), emqx_modbus_keepalive:is_timeout(X)).
-define(KEEPALIVE_CANCEL(X),  emqx_modbus_keepalive:cancel_timer(X)).
-define(KEEPALIVE_KICK(X),    emqx_modbus_keepalive:kick_timer(X)).

%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------

start_link(Transport, Sock) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [[Transport, Sock]])}.

send_request(Device, Req) ->
    gen_server:cast({via, ?MODBUS_VIA_MODULE, Device}, {send_request_to_device, Req}).


%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

init([Transport, Sock]) ->
    case Transport:wait(Sock) of
        {ok, NewSock} ->
            Transport:setopts(NewSock, [{active, once}]),
            Handler = application:get_env(?APP, handler, emqx_modbus_user_data),
            KeepaliveInterval = application:get_env(?APP, keepalive, 120),
            gen_server:enter_loop(?MODULE, [], #state{transport    = Transport,
                                                      sock         = NewSock,
                                                      handler      = Handler,
                                                      parser       = emqx_modbus_frame:init(),
                                                      msgid        = 0,
                                                      keepalive    = ?KEEPALIVE_START(KeepaliveInterval)});
        Error -> Transport:fast_close(Sock), Error
    end.

handle_call(Request, _From, State) ->
    ?LOG(error, "emqx_modbus_device_mode0, ignore unknown call=~p~n", [Request]),
    {reply, ok, State}.

handle_cast({send_request_to_device, Req=#modbus_req{}}, State=#state{}) ->
    NewState = process_request(Req, State),
    {noreply, NewState};

handle_cast(Msg, State) ->
    ?LOG(error, "emqx_modbus_device_mode0, ignore unknown cast=~p~n", [Msg]),
    {noreply, State}.

handle_info({Ok, _Sock, Data}, State = #state{transport = Transport, sock = Sock, parser = Parser, keepalive = Keepalive})
    when Ok =:= tcp; Ok =:= ssl ->
    NewKeepalive = ?KEEPALIVE_KICK(Keepalive),
    {ok, PeerName} = Transport:ensure_ok_or_exit(peername, [Sock]),
    ?LOG(info, "~s - ~s~n", [esockd_net:format(peername, PeerName), Data]),
    Transport:setopts(Sock, [{active, once}]),
    case emqx_modbus_frame:parse(Data, Parser) of
        {more_data, Parser1} ->
            {noreply, State#state{parser = Parser1, keepalive = NewKeepalive}};
        {ok, Frame=#modbus_frame{}, Parser2} ->
            process_modbus_frame(Frame, State),
            {noreply, State#state{parser = Parser2, keepalive = NewKeepalive}};
        {error, Parser3} ->
            {noreply, State#state{parser = Parser3, keepalive = NewKeepalive}};
        {id, #modbus_identity{id = NewDeviceName}, Parser4} ->
            kickout_present_device(NewDeviceName),
            emqx_modbus_registry:register_name(NewDeviceName, self()),
            {noreply, State#state{parser = Parser4, device_name = NewDeviceName, keepalive = NewKeepalive}};
        {keepalive_packet , Parser5} ->
            {noreply, State#state{parser = Parser5, keepalive = NewKeepalive}}
    end;

handle_info({Error, Sock, Reason}, State = #state{}) when Error =:= tcp_error; Error =:= ssl_error ->
    ?LOG(error, "tcp error, Sock=~p, Reason=~p", [Sock, Reason]),
    {stop, {shutdown, {tcp_error, Reason}}, State};

handle_info({Closed, Sock}, State = #state{}) when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    ?LOG(debug, "tcp closed, Sock=~p", [Sock]),
    {stop, normal, State};

handle_info({shutdown, Error}, State = #state{}) ->
    ?LOG(debug, "shutdown due to tcp error ~p", [Error]),
    {stop, normal, State};

handle_info({inet_reply, _Sock, ok}, State) ->
    {noreply, State};


handle_info(keepalive_timer, State=#state{keepalive = Keepalive}) ->
    case ?KEEPALIVE_TIMEOUT(Keepalive) of
        true ->
            ?LOG(debug, "emqx_modbus_keepalive timeout", []),
            {stop, normal, State};
        false ->
            ?LOG(debug, "emqx_modbus_keepalive restart", []),
            NewKeepAlive = ?KEEPALIVE_RESTART(Keepalive),
            {noreply, State#state{keepalive = NewKeepAlive}}
    end;


handle_info(Info, State) ->
    ?LOG(error, "emqx_modbus_device_mode0, ignore unknow info=~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{keepalive = Keepalive}) ->
    ?KEEPALIVE_CANCEL(Keepalive),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

process_modbus_frame(#modbus_frame{}, #state{device_name = undefined}) ->
    % if device is not registered, discard this modbus frame
    ok;
process_modbus_frame(#modbus_frame{header = #mbap_header{unit_id = UnitId}, funcode = Funcode, payload = ModbusData},
                     #state{device_name = DeviceName, handler = Handler, msgid = MsgId}) ->
    Rsp = #modbus_rsp{msgid = MsgId, device_name = DeviceName, uid = UnitId, funcode = Funcode, data = ModbusData},
    MqttPayload = Handler:handle_ul_data(Rsp),
    emqx_modbus_control:send_response(MqttPayload).

process_request(undefined, State) ->
    State;
process_request(Req=#modbus_req{msgid = MsgId, uid = UnitId, funcode = FunCode, data = Data},
    State=#state{transport = Transport, sock = Sock, parser = Parser}) ->
    ?LOG(debug, "handle_pending_req ~p", [Req]),
    {ModBusData, NewParser} = emqx_modbus_frame:serialize(UnitId, FunCode, Data, Parser),
    send_data(Transport, Sock, ModBusData),
    State#state{msgid = MsgId, parser = NewParser}.

send_data(Transport, Sock, Data) ->
    try Transport:async_send(Sock, Data) of
        true -> ok
    catch
        error:Error -> self() ! {shutdown, Error}
    end.

kickout_present_device(DeviceName) ->
    case emqx_modbus_registry:whereis_name(DeviceName) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    emqx_modbus_registry:unregister_name(DeviceName),
    ok.


