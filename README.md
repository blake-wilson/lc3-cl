## LC3 VM

This is a VM based on [Justin Meiner's great tutorial](https://www.jmeiners.com/lc3-vm/).
It's written in Common Lisp (tested only with SBCL).

### Requirements

This project requires [Quicklisp](https://www.quicklisp.org/beta/) to download package dependencies.
The GCC compiler is used for compiling the included C code.

### Building

Because common-lisp doesn't provide any libraries to interface with the
terminal directly, I've bundled the C file `keyboard.c` to do terminal IO.

To compile this C file, which is a dependency to run the VM, run `make` (Mac or Linux only supported).

To run the VM, load `vm.lisp` with your Lisp interpreter from this project's root directory:
```
~/src/github.com/blake-wilson/lc3-vm $sbcl --load "vm.lisp"
```

And run any assembled LC3 program via
```
* (run-image "bin/2048.obj")
```
(replace `bin/2048.obj` with any other LC3 object file you wish to run.)
