# Notes about the `CL3270` Library

The code has been rewritten to track Matthew R. Wilson's GO code
latest version (November 2025).


## 2025-12-31

### To Do

Reuse the socket address.


### SBCL

As usual, SBCL is very annoying.  Tracking down a mysterious type bug.


## 2025-11-30

### Rewriting

The code has been rewritten in a more **Common Lisp** way, to better
leverage the condition system.  The semantics should be the same of
the GO code.


### Codepages

The old file `ebcdic-ascii.lisp` has been superseded by the
`ebcdic.lisp` file and by the `codepage.lisp` file.  The actual
codepages, which are in the `internal/codepage/` folder are kept in
the `codepages/` folder.

