

# The exometer_http_yaws application #

This application allows quick inspection of the application which
uses exometer to store its metrics.

The appmod of this application streams metric values to URL when socket connection is
established. You may import this streamed .csv file to real-time data
visualization tool like kst, livegraph, etc.

Also, Multiple simultaneous connections are supported.


### <a name="Data_streaming">Data streaming</a> ###

There are multiple ways of getting a streamed .csv file.

To stream metrics to .csv file, run following command:

```
curl -N -get http://localhost:8004/ --verbose > ~/Desktop/kst_test_files/test2.csv
```

Where parameter `-N` means no buffering and `--verbose` provides some info to terminal.

Or try downloading file using browser.


### <a name="Configuration">Configuration</a> ###

This application configured in `sys.config` file and `yaws.conf`.

Configuration items under `sys.config`:
* delay - time interval between metric values retrieval from Exometer.
Provided in miliseconds. Default value is 1 second.

Example:

```
    {exometer_http_yaws, [
        {delay, 1000}
    ]}
```

Configuration items under `yaws.conf`:

* You may specify custom URL path.
Example:

```
<server exometer_http_yaws>
    ...
    appmods = </csv, exometer_http_yaws>
    ...
</server>
```


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/erisata/exometer_http_yaws/blob/master/doc/exometer_http_yaws.md" class="module">exometer_http_yaws</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_http_yaws/blob/master/doc/exometer_http_yaws_app.md" class="module">exometer_http_yaws_app</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_http_yaws/blob/master/doc/exometer_http_yaws_stream.md" class="module">exometer_http_yaws_stream</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_http_yaws/blob/master/doc/exometer_http_yaws_stream_sup.md" class="module">exometer_http_yaws_stream_sup</a></td></tr>
<tr><td><a href="http://github.com/erisata/exometer_http_yaws/blob/master/doc/exometer_http_yaws_sup.md" class="module">exometer_http_yaws_sup</a></td></tr></table>

