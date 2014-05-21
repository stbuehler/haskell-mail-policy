# Description

mail-policy is a haskell library to implement a policy daemon. See
<http://www.postfix.org/SMTPD_POLICY_README.html> for the protocol and
<http://www.postfix.org/access.5.html> for possible response actions.

It comes with support for:

* running a policy server
* making policy requests to another server
* querying DNS blacklists

# Example

    ghc -threaded -O3 -Wall example.hs
    ./example

By default it will listen to 127.0.0.1:10022, but you can either modify the
source or provide a listening socket on fd 0 (for example with spawn-fcgi).
