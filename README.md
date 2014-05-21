# Description

mail-policy is a haskell library to implement a policy daemon. See
<http://www.postfix.org/SMTPD_POLICY_README.html> for the protocol and
<http://www.postfix.org/access.5.html> for possible response actions.

It comes with support for:

* running a policy server
* making policy requests to another server
* querying DNS blacklists

# Building

Distribution maintainers should already know how to build cabal packages.

To install as user (requires ghc-7.6, cabal and some dependencies; `cabal
configure` will tell you what is missing):

    cabal configure --user --enable-tests
    cabal install

After updating you should also rebuild all binaries that use this library.

# Example

Add a restriction class in your postfix main.cf:

    smtpd_restriction_classes = check_policy_postgrey
    check_policy_postgrey =
        check_policy_service inet:127.0.0.1:10023

Also make it call the example (listening to 127.0.0.1:10022 by default):

    smtpd_recipient_restrictions =
        # ...
        # "mail-policy"
        check_policy_service inet:127.0.0.1:10022,
        # ...
        permit

The example will redirect some blacklist hits to this restriction class.

    ghc -threaded -O3 -Wall example.hs
    ./example &
    postfix reload

By default it will listen to 127.0.0.1:10022, but you can either modify the
source or provide a listening socket on fd 0 (for example with spawn-fcgi).
