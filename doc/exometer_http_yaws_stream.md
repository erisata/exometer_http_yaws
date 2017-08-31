

# Module exometer_http_yaws_stream #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Streams values of specified exometer metrics.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td>
Unused.</td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td>
Setups socket between client and Yaws.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>
Saves socket of the client and starts streaming procedure.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>
Start a streamer.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td>
Unused.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`

Unused.

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(Unknown, From, State) -> any()`

Unused.

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Unknown, State) -> any()`

Unused.

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Unknown, State) -> any()`

Setups socket between client and Yaws.
It waits for Yaws to confirm that the socket is ready for streaming.

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

Saves socket of the client and starts streaming procedure.

<a name="start_link-1"></a>

### start_link/1 ###

`start_link(Socket) -> any()`

Start a streamer.

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

Unused.

