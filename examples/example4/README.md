# CL3270 Example 4: Mock Database Application

This example is a bit more complicated and I found myself prefixing
many functions and names with `e4-`.  I therefore decided to handle it
as a separate **CL** system.

## Differences with Matthew Wilson's **GO** Code

**GO** has *methods* and *interfaces* and the `go3270` code exploits
*method pointers* a lot.  In **CL** you have *functions* (generic and
regular).  Therefore I had to change the signature of many of the
```go
    func (sess *session) trans(...) ...
```
to more **CL** style
```lisp
    (defun trans (sess ...) ...)
```
Things work as expected.


## Setup

The setup of this little example is the usual one with a `.asd` (or a
`.system`) file and a package nicknamed `CL3270.E4`.


# Enjoy

Marco Antoniotti
