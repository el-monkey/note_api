# Note API 

This Erlang/OTP application is the API application for the
[note](https://github.com/el-monkey/note) application.

It is the first API that I have attempted to write that uses other
Erlang libraries/applications that I have also written.

The application uses the following third party utility libraries

- [jiffy](https://github.com/davisp/jiffy) for rendering JSON
- [iso8601](https://github.com/erlsci/iso8601) for rendering ISO86 dates

The application also uses [Cowboy](https://github.com/ninenines/cowboy) to handle
the HTTP request.

Dependencies are managed using [Rebar3](https://www.rebar3.org/).

## Build

```bash
    $ rebar3 compile
```

## Testing the Api with Curl

```bash
$ curl -i -XGET 127.0.0.1:8080/notes
```

### Pretty print

```bash
$ curl  -XGET 127.0.0.1:8080/notes | python -m json.tool
```






Method not allowed

HTTP/1.1 405 Method Not Allowed
allow: HEAD, GET, OPTIONS
content-length: 0
date: Thu, 21 Sep 2017 10:24:46 GMT
server: Cowboy


[vagrant@localhost ~]$ curl -i -d text=mytext -XPOST 127.0.0.1:8080/notes
HTTP/1.1 415 Unsupported Media Type
content-length: 0
content-type: application/json
date: Thu, 21 Sep 2017 10:28:37 GMT
server: Cowboy

[vagrant@localhost ~]$ curl -v -d "text=This is my note" -H "Content=t-Type: application/x-www-form-urlencoded" -X POST 127.0.0.1:8080/notes
* About to connect() to 127.0.0.1 port 8080 (#0)
*   Trying 127.0.0.1...
* Connected to 127.0.0.1 (127.0.0.1) port 8080 (#0)
> POST /notes HTTP/1.1
> User-Agent: curl/7.29.0
> Host: 127.0.0.1:8080
> Accept: */*
> Content=t-Type: application/x-www-form-urlencoded
> Content-Length: 20
> Content-Type: application/x-www-form-urlencoded
>
* upload completely sent off: 20 out of 20 bytes
< HTTP/1.1 200 OK
< content-length: 15
< content-type: application/text
< date: Thu, 21 Sep 2017 11:10:57 GMT
< server: Cowboy
<
* Connection #0 to host 127.0.0.1 left intact
