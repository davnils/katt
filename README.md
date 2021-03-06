### katt - CLI for the Kattis judge system      [![Build Status](https://travis-ci.org/davnils/katt.png)](https://travis-ci.org/davnils/katt)

Unofficial [Kattis](https://www.kattis.com) CLI written in a git-like fashion, supports automatic
downloading of test cases, easy submissions, and more.

Basically a problem is initialized as follows
(creates a directory and downloads any available test cases):

    > katt init hello
    > cd hello

Source files, problem names, and language identification is handled 
automatically when submitting solutions:

    > vim Hello.java
    > katt submit
    Made submission 4712
    Accepted: 1 of 1 test(s) passed

There is also built-in support for problem sessions,
which initializes all problems in the problem list.

#### Command overview

    init <problem>
      Create the corresponding directory and download any available tests.

    init-session <session>
      Initialize all problems associated to the problem session, given as an integer id.

    submit [+add_file] [-skip_file]
      Make a submission using the problem name read from a local config.
      Defaults to recursively including all source and header files that can be found.
      Use the optional filter arguments to include or exclude files.


#### Installation

Get started by downloading a *.kattisrc* config from
the official site, make sure you have 
[the Haskell Platform](http://www.haskell.org/platform/) 
installed, then run *cabal install katt*.

There are also binaries available for x64 linux.  

Feel free to post an issue or pull request if there is something
buggy (very likely in beta) or some improvement.
Please note that the beta release is limited to C, C++ and Java.
It also only supports running on nix-like systems.
