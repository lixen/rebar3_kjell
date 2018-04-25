rebar3_kjell
=====

A rebar plugin adding support for [kjell](https://github.com/karlll/kjell)

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_kjell, ".*", {git, "git@github:lixen/rebar3_kjell.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 kjell
    ===> Fetching rebar3_kjell
    ===> Compiling rebar3_kjell

