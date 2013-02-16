cinterfaces
===========

C interfaces code generator written in Common Lisp

## description ##
This is lisp code that allows you to take a DSL specifying an interface then it will generate a .c and a .h file that implements the interface.  The example Makefile requires sbcl.

## example ##
DSL:
```lisp
(defcinterface RamRod
    (defcfunc void foobar ((int foo) (double bar)))
    (defcfunc double foobazBong ()))
```

Generated Header:
```C
#pragma once

struct RamRod;

typedef double (*RamRod_foobazBong_func)(struct RamRod* self);
typedef void (*RamRod_foobar_func)(struct RamRod* self, int foo, double bar);

typedef struct RamRod_vtable {
  RamRod_foobazBong_func foobazBong;
  RamRod_foobar_func foobar;
} RamRod_vtable;

typedef struct RamRod {
  RamRod_vtable vtable;
} RamRod;

double RamRodFoobazBong(RamRod* self);
void RamRodFoobar(RamRod* self, int foo, double bar);
```

Generated C:
```C
#include "RamRod.h"
double RamRodFoobazBong(RamRod* self) {
  self->vtable.foobazBong(self);
}
void RamRodFoobar(RamRod* self, int foo, double bar) {
  self->vtable.foobar(self, foo, bar);
}
```

## files ##
* cinterfaces.lisp : lisp code that does the generation
* compile.lisp : script wrapper around cinterfaces calls to make a nice shell tool
* cinterfaces.txt : example DSL file
* Makefile : example of how to integrate with the build system, will compile cinterfaces.txt

## limitations ##
Types with spaces (ex. "const char*") aren't yet implemented.
