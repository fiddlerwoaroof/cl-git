#.(handler-case (progn
                  (fwoar.repl-utils:clone "https://github.com/fiddlerwoaroof/fwoar.lisputils.git" "fwoar-lisputils")
                  (fwoar.repl-utils:github "fiddlerwoaroof" "cl-git")
                  (ql:quickload :cl-git)
                  (ql:quickload :cl-git/tests)
                  nil)
    (serious-condition (c)
      (format t "~&Exiting on condition: ~s~%" c)
      (sb-ext:exit :code 40)))

(handler-case (alexandria:if-let ((results (5am:run :fwoar.cl-git)))
                (sb-ext:exit
                 :code (if (5am:explain! results)
                           0
                           42))
                (sb-ext:exit :code 41))
  (serious-condition (c)
    (format t "~&Exiting on condition: ~s~%" c)
    (sb-ext:exit :code 43)))
