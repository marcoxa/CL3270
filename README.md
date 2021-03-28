CL3270 Server Library
=====================
Copyright (c) 2021 Marco Antoniotti
See file COPYING for licensing information


DESCRIPTION
-----------

A clone of the [go3270](https://github.com/racingmars/go3270) library
by Matthew R. Wilson.

The library follows very closely the GO code with some minimal **Common
Lisp** idioms (e.g., practically no use of the Condition System).

It is a good exercise for learning a number of things, including
[USOCKET](https://common-lisp.net/project/usocket/api-docs.shtml).

The library has been tested on W10 with [Lispworks](www.lispworks.com)
(a version using the native `COMM` package also works), but is should
be portable to other **Common Lisp** systems.


USAGE
-----

To start the server just issue the following:
```
CL-USER 42 > (asdf:load-system "cl3270")
;;; ...
T

CL-USER 43 > (in-package "CL3270")
#<The CL3270 package, 327/512 internal, 6/16 external>

CL3270 44 > (load "examples/example2.lisp")
#P"examples/example2.lisp")

CL3270 45 > (cl3270-example2 :debug nil)
```

I am assuming you moved in the correct directory and that you have
loaded the `.asd` file, etc. etc.

Now you can start [wx3270](http://x3270.bgp.nu/) terminal and connect
to `127.0.0.1` (`localhost` on W10 may not work) on port 3270.  You
should be in business...


### A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy at an acceptable level.


Enjoy.
