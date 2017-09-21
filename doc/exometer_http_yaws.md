

# Module exometer_http_yaws #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module serves two purposes:.

<a name="description"></a>

## Description ##
* It is a yaws appmod for streaming exometer statistics;
* It provides main API for this application.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#out-1">out/1</a></td><td>
Starts stream of Exometer metrics as csv values to the client socket.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
This is needed to be able to start this application from console,
using <code>-s exometer_http_yaws</code> option.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="out-1"></a>

### out/1 ###

`out(Arg) -> any()`

Starts stream of Exometer metrics as csv values to the client socket.

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

This is needed to be able to start this application from console,
using `-s exometer_http_yaws` option.

