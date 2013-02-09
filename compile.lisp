#!/usr/bin/env sbcl --script

(if (eql (length *posix-argv*) 4)
    (progn 
      (load "cinterface.lisp")
      (write-cinterface (nth 1 *posix-argv*) (nth 2 *posix-argv*) (nth 3 *posix-argv*)))
    (format t "usage: compile.lisp <inteface file> <header file> <src file>~%"))
