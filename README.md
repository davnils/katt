sofie
=====

Unofficial CLI client for the Kattis online judge system.

Written in a git-like fashion, it supports automatic
downloading of test cases, easy submissions, and more.

Basically a problem is initialized as follows
(downloads any available test cases):

    > sofie init hello

Source files, problem names, and language identification is handled 
automatically when submitting solutions:

    > sofie submit
    Made submission 4712
    Accepted: 1 of 1 test(s) passed

There is also built-in support for problem sessions,
which initalizes all problems in the problem list.

Get started by downloading a *.kattisrc* config from
the official site, make sure you have the haskell platform
installed, then run *cabal install sofie*.
