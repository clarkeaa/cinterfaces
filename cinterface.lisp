;(defpackage cinterface (:use :cl) (:export :write-cinterface))
;(in-package :cinterface)

;(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *interface* nil)

(defclass method-arg () 
  ((type :initarg :type)
   (name :initarg :name)))
(defclass interface-method () 
  ((name :initform "") 
   (return-type :initform "void")
   args))
(defclass interface () 
  ((name :initform "") 
   (methods :initform nil)))

(defmacro format-lines (stream &rest lines)
  `(progn
     ,@(loop for x in lines collect 
            `(progn (format ,stream ,@x) (format ,stream "~%")))))

(defun calc-func-sig (interface-name method-name)
  (format nil "~a_~a_func" interface-name method-name))

(defun write-args (stream args)
  (if args
      (let ((args-strings (mapcar 
                           (lambda (arg) 
                             (with-slots (name type) arg
                               (format nil "~a ~a" type name)))
                           args)))
        (format stream (reduce 
                        (lambda (x y) (format nil "~a, ~a" x y)) 
                        args-strings)))
      (format stream "void")))

(defun write-func-typedef (method interface-name stream)
  (with-slots (name return-type args) method
    (format stream "typedef ~a (*~a)(" 
            return-type
            (calc-func-sig interface-name name))
    (let* ((self-type (format nil "struct ~a*" interface-name))
           (self-arg (make-instance 'method-arg :type self-type :name "self")))
      (write-args stream (cons self-arg args)))
    (format stream ");~%")))

(defun write-vtable-ivar (method interface-name stream)
  (with-slots (name) method
    (format stream "  ~a ~a;~%" (calc-func-sig interface-name name) name)))

(defun capitalize (str)
  (let* ((first-char (char str 0))
         (cap-char (string-upcase first-char)))
    (concatenate 'string cap-char (subseq str 1))))

(defun write-function-sig (method interface-name stream)
  (with-slots (name return-type args) method
    (format stream "~a ~a~a(" return-type interface-name (capitalize name))
    (let* ((self-type (concatenate 'string interface-name "*"))
           (self-arg (make-instance 'method-arg :type self-type :name "self")))
      (write-args stream (cons self-arg args))))
  (format stream ")"))

(defun write-header (int stream)
  (with-slots (name methods) int 
    (format-lines stream 
                  ("#pragma once~%")
                  ("struct ~a;~%" name))
    (loop for method in methods do
         (write-func-typedef method name stream))    
    (format-lines stream
                  ("")
                  ("typedef struct ~a_vtable {" name))
    (loop for method in methods do
         (write-vtable-ivar method name stream))
    (format-lines stream
                  ("} ~a_vtable;" name)
                  ("")
                  ("typedef struct ~a {" name)
                  ("  ~a_vtable vtable;" name)
                  ("} ~a;" name)
                  (""))
    (loop for method in methods do
         (write-function-sig method name stream)
         (format stream ";~%"))))

(defun write-function-call-args (method stream)
  (let* ((arg-names (cons "self" 
                         (mapcar (lambda (x) (slot-value x 'name)) 
                                 (slot-value method 'args))))
         (str (reduce 
               (lambda (x y) (format nil "~a, ~a" x y)) 
               arg-names)))
    (format stream str)))

(defun write-source (int stream)
  (with-slots (name methods) int 
    (format stream "#include \"~a\.h\"~%" name)
    (loop for method in methods do
         (write-function-sig method name stream)
         (format stream " {~%")
         (format stream "  self->vtable.~a(" (slot-value method 'name))
         (write-function-call-args method stream)
         (format-lines stream 
                       (");")
                       ("}")))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun gen-arg (cons-arg)
  (let ((answer (make-instance 'method-arg)))
    (with-slots (type name) answer
      (setf type (first cons-arg))
      (setf name (second cons-arg)))
    answer))

(defun gen-args (cons-args)
  (if (eql cons-args nil)
      nil
      (cons (gen-arg (first cons-args)) (gen-args (rest cons-args)))))

(defun add-c-func (int return-type-arg name-arg args-arg)
  (let ((new-method (make-instance 'interface-method)))
    (with-slots (return-type name args) new-method
      (setf name name-arg)
      (setf return-type (symbol-name return-type-arg))
      (setf args (gen-args args-arg)))
    (with-slots (methods) int
      (setf methods (cons new-method methods)))))

(defmacro |defcfunc| (return-type name args)
  `(add-c-func *interface* ',return-type ,(symbol-name name) ',args))

(defmacro |defcinterface| (name &rest body)
  `(progn
     (setf *interface* (make-instance 'interface))
     (setf (slot-value *interface* 'name) ,(symbol-name name))
     ,@body
     *interface*))     

(defmacro with-preserve-reader (&rest body)
    `(unwind-protect 
          (progn 
            (setf (readtable-case *readtable*) :preserve)
            ,@body)
       (setf (readtable-case *readtable*) :upcase)))
       
(defun gen-cinterface (path header-stream source-stream)
  (let ((data nil))
    (with-preserve-reader 
        (setf data (with-open-file (in path) (read in))))
    (let ((int (eval (macroexpand data))))
      (write-header int header-stream)
      (write-source int source-stream)
      t)))

(defun write-cinterface (interface-path hpath cpath)
  (with-open-file (header hpath :if-exists :supersede :direction :output)
    (with-open-file (src cpath :if-exists :supersede :direction :output)
      (gen-cinterface interface-path header src))))
