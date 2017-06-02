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

-module(emq_modbus_device).

-include("emq_modbus.hrl").

%% API Exports
-export([connect/3, disconnect/1, send_request/2]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    server_addr  :: term(),
         socket  :: inet:socket(),
  timeout_count  :: integer(),
    device_name  :: binary(),
    pending_req  :: undefined | term(),
        tid = 1  :: 1..16#ff
 }).

-define(CALL_TIMEOUT, 60000).

-define(SOCK_TIMEOUT, 5000).

-define(RECV_TIMEOUT, 10000).

-define(TIMEOUT_MAX, 5).

%% Modbus Socket Options
-define(SOCKOPTS, [
        binary,
        {active, false},
        {packet, 0},
        {reuseaddr, true},
        {nodelay,   true}
]).

%%--------------------------------------------------------------------
%% Exported APIs
%%--------------------------------------------------------------------

connect(Host, Port, DeviceName) ->
    DeviceNameBinary = device_name(DeviceName),
    gen_server:start_link({via, ?MODBUS_VIA_MODULE, DeviceNameBinary}, ?MODULE, [Host, Port, DeviceNameBinary], []).

disconnect(Device) ->
    cast(Device, stop).

send_request(Device, Req) ->
    cast(Device, {send_request, Req, self()}).


%%--------------------------------------------------------------------
%% gen_server Callbacks
%%--------------------------------------------------------------------

call(Device, Request) ->
    gen_server:call({via, ?MODBUS_VIA_MODULE, device_name(Device)}, Request).

cast(Device, Request) ->
    gen_server:cast({via, ?MODBUS_VIA_MODULE, device_name(Device)}, Request).


init([Host, Port, DeviceName]) ->
    true = is_binary(DeviceName),
    ?LOG(debug, "start device ~p", [DeviceName]),
    self() ! reconnect,  % DO not call gen_tcp:connect() here, since it may be stuck.
    {ok, #state{socket = undefined, timeout_count = 0,
                 device_name = DeviceName, server_addr = {Host, Port}}}.


handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State, hibernate}.

handle_cast({send_request, Req, From}, State=#state{socket = undefined})  ->
    {noreply, State#state{pending_req = {Req, From}}};
handle_cast({send_request, Req, From}, State)  ->
    NewState = request_and_response(Req, From, State),
    {noreply, next_id(NewState)};

handle_cast(stop, State=#state{socket = undefined}) ->
    {stop, normal, State};
handle_cast(stop, State=#state{socket = Sock}) ->
    gen_tcp:close(Sock),
    {stop, normal,State};


handle_cast(Msg, State) ->
    ?LOG(error, "emq_modbus_device, ignore unknow msg=~p", [Msg]),
    {noreply, State, hibernate}.

handle_info(reconnect, State=#state{device_name = DevinceName,
                                      pending_req = PendingReq,
                                      server_addr = {Host, Port}}) ->
    case connect_tcp(Host, Port) of
        {ok, Sock} ->
            ?LOG(info, "success connected ~p ~p:~p", [DevinceName, Host, Port]),
            NewState = handle_pending_req(PendingReq,
                                          State#state{socket = Sock,
                                                       timeout_count = 0,
                                                       pending_req = undefined}),
            {noreply, NewState};
        {error, _Error} ->
            ?LOG(error, "fail to connect ~p ~p:~p", [DevinceName, Host, Port]),
            NewState = reconnect_tcp(false, State),
            {noreply, NewState, hibernate}
    end;

handle_info({suback, _, _}, State) ->
    {noreply, State};

handle_info({subscribe, _}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG(error, "emq_modbus_device, ignore unknow info=~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

connect_tcp(Host, Port) ->
    gen_tcp:connect(Host, Port, ?SOCKOPTS, ?SOCK_TIMEOUT).

reconnect_tcp(false, State) ->
    erlang:send_after(10, self(), reconnect),
    State#state{socket = undefined};
reconnect_tcp(true, State) ->
    self() ! reconnect,
    State#state{socket = undefined}.

request_and_response(Req=#modbus_req{msgid = MsgId}, From, State=#state{device_name = DeviceName}) ->
    % If the slave does not receive the query due to a communication error, no response is returned.
    % The master program will eventually process a timeout condition for the query.
    case catch send_and_recv(Req, State) of
        #modbus_frame{funcode = Funcode, payload = Data} ->
            Resp = #modbus_rsp{device_name = DeviceName, msgid = MsgId,
                funcode = Funcode, data = Data},
            emq_modbus_control:send_response(From, Resp),
            State#state{timeout_count = 0};
        {'EXIT', Error} ->
            ?LOG(error, "sending Request get error ~p", [Error]),
            State;
        {error, closed} ->
            ?LOG(error, "socket is closed abnormaly", []),
            reconnect_tcp(true, State#state{pending_req = {Req, From}});
        {error, timeout} ->
            handle_timeout(State);
        {error, Reason} ->
            ?LOG(error, "send request get error: ~p~n", [Reason]),
            State
    end.

handle_timeout(State=#state{timeout_count = ?TIMEOUT_MAX}) ->
    ?LOG(debug, "socket has too many timeout", []),
    reconnect_tcp(true, State);
handle_timeout(State=#state{timeout_count = Count}) ->
    State#state{timeout_count = Count+1}.

handle_pending_req(undefined, State) ->
    State;
handle_pending_req({Req, From}, State) ->
    ?LOG(debug, "handle_pending_req ~p", [Req]),
    request_and_response(Req, From, State).

send_and_recv(_Req, #state{socket = undefined}) ->
    {error, "server has not been connected!"};

send_and_recv(Req, State) ->
    case send(Req, State) of
        ok ->
            case recv(State) of
                {ok, Frame} ->
                    Frame;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.



send(#modbus_req{funcode = FunCode, data = Data}, #state{tid = Tid, socket = Sock}) ->
    Hdr= #mbap_header{tid = Tid, unit_id = 0},
    Frame = #modbus_frame{header = Hdr, funcode = FunCode, payload = Data},
    ModBusData = serialize(Frame),
    gen_tcp:send(Sock, ModBusData).

serialize(#modbus_frame{header = #mbap_header{tid = Tid, unit_id = Uid},
                          funcode = FunCode, payload = Payload}) ->
    Len = 2 + size(Payload),
    <<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>.
    
recv(State) ->
    case recv(header, State) of
        {ok, Header} ->
            recv(payload, Header, State);
        {error,Error} ->
            {error,Error}
    end.

recv(header, #state{tid = Tid, socket = Sock}) ->
    case gen_tcp:recv(Sock, ?MBAP_LENGTH, ?RECV_TIMEOUT) of
        {ok, <<Tid:16, 0:16, Len:16>>} ->
            {ok, #mbap_header{tid = Tid, length = Len}};
        {ok, Header} ->
            ?LOG(error, "Response cannot match request: request tid=~p, response header =~p", [Tid, Header]),
            {error, badresp};
        {error, Reason} ->
            {error, Reason}
    end.

recv(payload, Header = #mbap_header{length = Len}, #state{socket = Sock}) ->
    case gen_tcp:recv(Sock, Len, ?RECV_TIMEOUT) of
        {ok, <<UnitId:8, FunCodeOrErr:8, Payload/binary>>} ->
            {ok, #modbus_frame{header = Header#mbap_header{unit_id = UnitId},
                               funcode = FunCodeOrErr, payload = Payload}};
        {ok, <<_:8>>}      ->  {error, too_short_modbus_payload};
        {error, Reason}    -> {error, Reason}
    end.

next_id(State = #state{tid = Tid}) when Tid >= 16#EFFF ->
    State#state{tid = 1};

next_id(State = #state{tid = Tid}) ->
    State#state{tid = Tid+1}.



device_name(DeviceName) when is_atom(DeviceName) ->
    atom_to_binary(DeviceName, utf8);
device_name(DeviceName) when is_list(DeviceName) ->
    list_to_binary(DeviceName);
device_name(DeviceName) when is_binary(DeviceName) ->
    DeviceName.


