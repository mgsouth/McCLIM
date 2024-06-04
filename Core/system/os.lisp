;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2024 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defun run-program (program arguments &rest parameters
                    &key input output error wait &allow-other-keys)
  (declare (ignore input output error wait))
  #+abcl  (apply #'sys:run-program program arguments parameters)
  #+ccl   (apply #'ccl:run-program program arguments parameters)
  #+clasp (apply #'ext:run-program program arguments parameters)
  #+clisp (apply #'ext:run-program program arguments parameters)
  #+cmu   (apply #'ext:run-program program arguments parameters)
  #+ecl   (apply #'ext:run-program program arguments parameters)
  #+sbcl  (apply #'sb-ext:run-program program arguments :search t parameters)
  #-(or abcl ccl clasp clisp cmu ecl sbcl)
  (error "PORTME: it is your turn, space cowboy!"))
