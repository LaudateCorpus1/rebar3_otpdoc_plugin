rebar3_otpdoc_plugin
=====

A rebar3 plugin for building OTP documentation

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_otpdoc_plugin, {git, "https://github.com/erlang/rebar3_otpdoc_plugin.git", {branch, "master"}}}
    ]}.


Generate documentation by calling `doc` under namespace `otp`.

    $ rebar3 otp doc
    ===> Generating specification-files
    ===> Installing html-files
    ===> Installing man-files

Options
-------

Useful rebar.config options.

    {otpdoc_opts, [{doc_src, string()},           %% default: $APP/doc/src
                   {company, string()},           %% default: "Ericsson AB"
                   {edoc_modules, [atom()]},      %% default: []
                   {edoc_chapters, [string()]},   %% default: [], ex.: "doc/overview.edoc"
                   {images, [string()]}           %% default: $APP/doc/src/**.{gif,jpeg,jpg,png}
                  ]}.
