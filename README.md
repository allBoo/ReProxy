ReProxy
=====

ReProxy is a proxy tunnel for multiple TOR proxy instances.

All clients are connected to a single ReProxy instance via SOCKS5 and each request from them is routed via unique TOR instance. 

Build
-----

    $ rebar3 compile
