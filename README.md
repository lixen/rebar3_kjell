rebar3_kjell
=====

A rebar plugin adding support for [kjell](https://github.com/karlll/kjell)

Just hacked a copy of the default shell to use kjell.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_kjell, {git, "https://github.com/lixen/rebar3_kjell.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 kjell
    ===> Fetching rebar3_kjell
    ===> Compiling rebar3_kjell

