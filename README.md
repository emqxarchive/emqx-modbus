emq_modbus
========

Modbus-TCP Gateway for the EMQ Broker

Configure Plugin
----------------

File: etc/emq_modbus.config
```
[
    {emq_modbus, [
            {company, "CompanyX"},
            {edgename, "EdgeY"},
            {qos, 1},
            {retain, false},
            {device, [
                        {"192.168.0.2", 502, "motor"},
                        {"192.168.0.9", 502, "thermo"}
                    ]
            }
        ]
    }
].
```

Gateway will subscribe a topic "/CompanyName/modbus_request/EdgeName/+". Control PC would reach device by publishing a message with topic="/CompanyName/modbus_request/EdgeName/DeviceName".
Device will responds with a PUBLISH message with topic="/CompanyName/modbus_response/EdgeName/DeviceName". This implies Control PC need to subscribe a topic "/CompanyName/modbus_response/EdgeName/+".

Payload of every message is described below:
```
   +--------------------------------+
   | MsgId | FunCode |     Data     |
   +--------------------------------+
    1 byte   1 byte     n bytes
```
MsgId is an integer which identify request-response pair.
FunCode is the modbus function code.
Data is modbus payload.


License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng@emqtt.io>


