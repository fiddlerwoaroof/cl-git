#.(progn
    (fwoar.repl-utils:clone "https://github.com/fiddlerwoaroof/fwoar.lisputils.git" "fwoar-lisputils")
    (fwoar.repl-utils:github "fiddlerwoaroof" "cl-git")
    (ql:quickload :cl-git)
    (ql:quickload :cl-git/tests)
    nil)

(5am:run-all-tests)

