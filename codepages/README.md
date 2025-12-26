CL3270 Server Library: Codepages
================================
Copyright (c) 2021-2026 Marco Antoniotti
See file COPYING for licensing information


The present directory, `codepages` contains the code to handle,
generate and re-generate the **CL** rendition of the
[UNICODE](https://home.unicode.org) `.ucm` files.

The **CL** codepages reside in the sub-directory `codepages/cps` and
are generated from a subset of the [ICU Data UCM Charset](https://github.com/unicode-org/icu-data/tree/main/charset/data/ucm)
data from [Github](https://www.github.com).

The subset of `.ucm` files is manually curated.

* First the `ibm-*.ucm` files, corresponding to the known
  [IBM](https://www.ibm.com) codepages were collected from the
  [ICU Data UCM Charset](https://github.com/unicode-org/icu-data/tree/main/charset/data/ucm)
  repository.  These files are held in the `codepages/icu-data-ibm`
  directory; the file `codepages/icu-data-ibm-ucm-files.txt` contains
  a list of them.
* Then a subset of them was selected for **CL** code generation (cf.,
  the file `setup.lisp` and the `ensure-codepages` function in
  `generate.lisp` in the present directory).  The set of pages that
  are generated is held -- for the time being -- in the variable
  `*ibm-x3270-icu-code-page-codes*`, defaulting to
  ```Lisp
  (list 37 273 275 277 278 280 284 285 297 424 500 803 870 871 875 880
        1026 1047 1140 1141 1142 1143 1144 1145 1146 1147 1148
        1149 1160)
  ```
  according to Matthew Wilson's Discord post.
* Finally the `ensure-codepages` functions populates the
  `codepages/cps` with the `lisp` files containing the
  `cl3270:codepage` structures.
  
As per Matthew Wilson's code, the **bracket** and the *graphics
escapes* **310** codepages are treated specially (vagaries of the
[x3270](https://x3270.bgp.nu/) emulator).


### A NOTE ON FORKING

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.  It helps
keeping the entropy at an acceptable level.


Enjoy.
