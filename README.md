rebar3_otpdoc_plugin
=====

A rebar3 plugin for building OTP documentation

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_otpdoc_plugin, ".*", {git, "git@host:user/rebar3_otpdoc_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_otpdoc_plugin
    ===> Fetching rebar3_otpdoc_plugin
    ===> Compiling rebar3_otpdoc_plugin
    <Plugin Output>
