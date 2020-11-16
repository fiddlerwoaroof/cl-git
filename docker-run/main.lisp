#.(progn
    (fwoar.repl-utils:clone "https://github.com/fiddlerwoaroof/fwoar.lisputils.git" "fwoar-lisputils")
    (fwoar.repl-utils:github "fiddlerwoaroof" "cl-git")
    (ql:quickload :cl-git)
    (ql:quickload :cl-git/tests)
    nil)

(sb-ext:exit
 :code (if (5am:explain! (5am:run :fwoar.cl-git))
           0
           42))
