
emqx_modbus
===========

EMQ X Modbus-TCP Gateway.

Configuration
-------------

File: etc/emqx_modbus.conf

```
modbus.mode = 0
modbus.company = CompanyX
modbus.port = 502
modbus.keepalive = 120

# modbus device list, format: DEV_NAME,IP,PORT
modbus.device.1 = motor,192.168.0.2,502
modbus.device.2 = thermo,192.168.0.5,502
```
- modbus.mode
  + mode 0: run EMQ as a modbus tcp server.
  + mode 1: run EMQ as a modbus tcp client, and modbus devices are tcp servers.
- modbus.company
  + a string represents your company name, and it will form the prefix of mqtt topic.
- modbus.keepalive
  + keepalive interval for tcp connection, in seconds.
- modbus.port
  + tcp port number that EMQ is listening on.
  + only available in mode 0.
- modbus.device.X
  + this parameter only exist in mode1.
  + there may be several devices.
  + DEV_NAME: device name, a string
  + IP: device's ip address.
  + port: modbus-tcp port number.
  
Message Flow
------------

* Gateway will subscribe a topic "{?CompanyName}/modbus_request".
* Control PC would reach device by publishing a message with topic="{?CompanyName}/modbus_request", and specify device name in json object.
* Gateway will translate the json object into modbus-tcp packet, and send it to device.
* Device will get a modbus-tcp request, and replay a modbus-tcp response.
* Gateway will translate the modbus-tcp response into a json object and put it into a PUBLISH message with topic="{?CompanyName}/modbus_response".
* Control PC will get a mqtt message with topic "{?CompanyName}/modbus_response".
> NOTE: Replace {?CompanyName} with your true name.

Message Format
--------------

MQTT message payload is a json object like below:

```
   {
       "device":"010203040506",
       "uid":2,
       "msgid":37,
       "funcode":1,
       "data":"BxQeKA=="
   }
```

* device is modbus device's identity
* uid is a integer, and only valid if device is a modbus gateway.
* msgid is an integer which identify request-response pair.
* funcode is the modbus function code.
* Data is modbus payload, which is base64 encoded.


Device Identity
--------------
Device has to report its identity in mode 0. All modbus frame will be dropped if device's identity is unknown. Device registers its identity in following format:
```
+-----------------------------------------------------+
| C3 | 25 | 9D | A9 |    identity (12 ascii chars)    |
+-----------------------------------------------------+
```

Keepalive
--------------
In mode 0, device has to send keepalive packet to keep tcp connection. Keepalive format:
```
+-------------------+
| C3 | 25 | 9D | AA |
+-------------------+
```


Kick-out
--------------
In mode 0, a device start a new tcp connection and emq-modbus will terminate the old tcp connection. This applies to two devices with same identity, that is to say, a device will kick out another online device with the same identity.



License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>

