
(in-package #:cl-user)

#+(or sbcl ecl allegro) (require :asdf)
#-(or sbcl ecl allegro)
(let ((*compile-print* nil)
      (*compile-verbose* nil)
      #+cmu (ext:*gc-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    (load #p"/usr/share/common-lisp/source/asdf/asdf.lisp"
          :print nil :verbose nil)))

(pushnew #p"/usr/share/common-lisp/systems/" asdf:*central-registry*)

(let ((*compile-print* nil)
      (*compile-verbose* nil)
      #+cmu (ext:*gc-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    (asdf:oos 'asdf:load-op :asdf-binary-locations)))
(setf asdf:*centralize-lisp-binaries* t)

