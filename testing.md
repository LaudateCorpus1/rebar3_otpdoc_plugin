Testing doc TODOs
=================

### Executing tests

Rebar3 does not start the Erlang distribution by default. This has to be set via CLI or in the `rebar.config`.

Different test specifications has to be choosen with profiles.

    {profiles, [{test, [{ct_opts, [{spec, "test/ssl.spec"}]},
                        {dist_node, [{sname, 'otptest'},
                                     {setcookie, mycookie}]}
                       ]},
                {bench, [{ct_opts, [{spec, "test/ssl_bench.spec"}]},
                         {dist_node, [{sname, 'otptest'},
                                      {setcookie, mycookie}]}
                        ]}
               ]}.

`rebar3 ct` will use the `test` profile, thus execute `test/ssl.spec` and start the distribution.

### Test specifications

`test/${app}.spec` needs to be rewritten for all applications.

Currently only paths in a test spec. points to a very OTP specific directory.

    {suites,"../mnesia_test",[mnesia_SUITE]}.

Needs to be rewritten to

    {suites,".",[mnesia_SUITE]}.

And similar for all other specifications.



