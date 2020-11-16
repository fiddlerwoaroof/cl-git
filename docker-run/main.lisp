#.(handler-case (progn
                  (fwoar.repl-utils:clone "https://github.com/fiddlerwoaroof/fwoar.lisputils.git" "fwoar-lisputils")
                  (fwoar.repl-utils:github "fiddlerwoaroof" "cl-git")
                  (ql:quickload :cl-git)
                  (ql:quickload :cl-git/tests)
                  nil)
    (serious-condition (c)
      (format t "~&Exiting on condition: ~s~%" c)
      (sb-ext:exit :code 40)))

(handler-case (sb-ext:exit
               :code (if (5am:explain! (5am:run :fwoar.cl-gir))
                         0
                         42))
  (serious-condition (c)
    (format t "~&Exiting on condition: ~s~%" c)
    (sb-ext:exit :code 43)))
